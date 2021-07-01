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

total_cost=0

# Looks like AWS treats support plan costs separately per account so
# calculating support costs for each account and adding up should be
# sufficient:
# https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/consolidatedbilling-support.html

account_ids=(981093152883 800864774051 942315655332 643696428705 104920772615 958251351375 112256684301 033027505565)
accounts="gra rate-platform rate-gri rate-affinity rate-proper rate-saas rate-edp rate-analytics"

total_support_cost=0
i=0
for a in $accounts
do
    account_monthly_cost=$(aws --profile $a ce get-cost-and-usage --time-period 'Start=2021-02-01,End=2021-03-01' --granularity MONTHLY --metrics UnblendedCost | jq --raw-output '.ResultsByTime[0].Total.UnblendedCost.Amount' | awk '{print int($1)}')
    # https://aws.amazon.com/premiumsupport/pricing/
    monthly_support_upgrade_cost=100
    support_cost_multiplier=0.03
    if [ $account_monthly_cost -lt 10000 ]
    then
	support_cost_multiplier=0.10
    elif [ $account_monthly_cost -lt 80000 ]
    then
	support_cost_multiplier=0.05
    fi
    support_cost=100
    possible_cost=$(echo - | awk "{print int($account_monthly_cost * $support_cost_multiplier)}")
    if [ $possible_cost -gt $support_cost ]
    then
	support_cost=$possible_cost
    fi
    echo "${account_ids[$i]},$account_monthly_cost,$support_cost"
    total_support_cost=$((total_support_cost + support_cost))
    i=$((i+1))
done
echo $total_support_cost
