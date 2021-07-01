# https://kvz.io/bash-best-practices.html

#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

failure() {
  local lineno=$1
  local msg=$2
  echo "Failed at $lineno: $msg"
}
trap 'failure ${LINENO} "$BASH_COMMAND"' ERR

export VAULT_ADDR=https://vault.platform.rate.com
export VAULT_TOKEN=$1

# creates new IAM credentials and "returns" the value in these global
# variables.
g_aws_access_key=RANDOM_INITIAL_VALUE
g_aws_secret_key=RANDOM_INITIAL_VALUE
create_new_aws_key() {
    echo "CREATING NEW KEY FOR $aws_iam_user"
    aws_profile=$1
    aws_iam_user=$2
    creds=$(aws --profile "$aws_profile" iam create-access-key --user-name "$aws_iam_user")
    echo $creds
    g_aws_access_key=$(echo "$creds" | jq --raw-output '.AccessKey.AccessKeyId')
    g_aws_secret_key=$(echo "$creds" | jq --raw-output '.AccessKey.SecretAccessKey')
}

# Updates the vault secret with a new AWS access key and secret. The
# old AWS access key gets "returned" in this global variable so that
# it can later be deactivated.
g_old_aws_access_key=RANDOM_INITIAL_VALUE
update_vault_secret() {
    secret_path=$1
    access_key_value=$2
    secret_key_value=$3
    access_key_key_name=${4:-AWS_ACCESS_KEY_ID}
    secret_key_key_name=${5:-AWS_SECRET_ACCESS_KEY}
    vault kv get -format json "$secret_path" | jq '.data' > old.json
    # Unfortunately we can't use
    # https://www.vaultproject.io/docs/commands/kv/patch so we need to
    # do this kind of hackery to make sure we don't delete the
    # existing secrets.
    cat old.json | jq '.'$access_key_key_name'="'${access_key_value}'" | .'$secret_key_key_name'="'${secret_key_value}'"' > new.json
    echo "CURRENT:"
    cat old.json
    echo "NEW:"
    cat new.json
    s=10
    echo "SLEEPING FOR $s SECONDS. VERIFY THAT THE SECRET UPDATE LOOKS OKAY AND CTRL-C IF IT DOES NOT"
    sleep $s
    vault kv put "$secret_path" @new.json
    g_old_aws_access_key=$(cat old.json | jq --raw-output ".$access_key_key_name")
}

# Rolls the k8s deployment by deleting pods one by one.
roll_k8s_deployment() {
    kubectl_cmd_start=$1
    deployment_name=$2
    pods_in_deployment=$($kubectl_cmd_start get pods --template '{{range .items}}{{.metadata.name}}{{"\n"}}{{end}}' | grep "$deployment_name")
    s=10
    echo "IN $s SECONDS WILL ROLL THESE THESE PODS BY DELETING THEM ONE BY ONE. CTRL-C AT ANY TIME IF IT LOOKS BAD"
    echo "$pods_in_deployment"
    sleep $s
    for p in $pods_in_deployment
    do
	$kubectl_cmd_start delete pod $p
	$kubectl_cmd_start rollout status deployment "$deployment_name"
	sleep 10
    done
}

# Deactivates an AWS access key
deactivate_old_key() {
    aws_profile=$1
    aws_iam_user=$2
    access_key=$3
    echo "DEACTIVATING KEY $access_key for user $aws_iam_user"
    aws --profile $aws_profile iam update-access-key --user-name $aws_iam_user --status Inactive --access-key-id $access_key
}

# Utility function combining the above functions into one which will
# rotate an AWS key that lives in ONE location within vault.
rotate_aws_key_in_single_vault_location() {
    aws_profile=$1
    aws_iam_user=$2
    vault_secret_path=$3
    kubectl_cmd_start=$4
    deployment_name=$5
    access_key_key_name=${6:-AWS_ACCESS_KEY_ID}
    secret_key_key_name=${7:-AWS_SECRET_ACCESS_KEY}
    create_new_aws_key "$aws_profile" "$aws_iam_user"
    update_vault_secret "$vault_secret_path" "$g_aws_access_key" "$g_aws_secret_key" "$access_key_key_name" "$secret_key_key_name"
    roll_k8s_deployment "$kubectl_cmd_start" "$deployment_name"
    deactivate_old_key "$aws_profile" "$aws_iam_user" "$g_old_aws_access_key"
}

########################
# GRI RED KEYS
########################

# My test call to make sure this works:

# rotate_aws_key_in_single_vault_location \
#     "root" \
#     "go-kubeconfig-generator-dev" \
#     "secret/dev/devops/go-kubeconfig-generator" \
#     "kubectl --context k8s.platform.rate.com --namespace devops" \
#     "go-kubeconfig-generator"

echo ROTATING ACCESS KEY AKIAY3UYVGBIIV4AY3NM
rotate_aws_key_in_single_vault_location \
    "root" \
    "scoreboard-dev" \
    "secret/scoreboard/gri/dev" \
    "kubectl-v1.8- --context gr18dev --namespace vega-dev" \
    "scoreboard-dev" \
    "aws-access-key-id" \
    "aws-secret-access-key"

echo ROTATING ACCESS KEY AKIAY3UYVGBIKPDSRGH6
aws_profile=root
aws_iam_user=mockingbird
create_new_aws_key "$aws_profile" "$aws_iam_user"
update_vault_secret "secret/mockingbird-service/gri/dev" "$g_aws_access_key" "$g_aws_secret_key" "aws/access-key" "aws/secret-key"
roll_k8s_deployment "kubectl-v1.8 --context gr18dev --namespace sig-dev" "mockingbird-service-dev"
update_vault_secret "secret/mockingbird-service/gri/prod" "$g_aws_access_key" "$g_aws_secret_key" "aws/access-key" "aws/secret-key"
roll_k8s_deployment "kubectl-v1.8 --context gr18prod --namespace sig-prod" "mockingbird-service-prod"
deactivate_old_key "$aws_profile" "$aws_iam_user" "$g_old_aws_access_key"
