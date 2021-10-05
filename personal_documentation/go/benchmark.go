// Benchmarks is a feature of golang which can get run via "go test"
// https://pkg.go.dev/testing#hdr-Benchmarks

// I copied the AD functions from jarvis.

// To run these benchmarks I:
// - compiled the test binary: GOOS=linux go test -c
// - Copied it to an EC2 instance in AWS
// - Ran the binary: <binary> -test.bench .
package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/labstack/gommon/log"
	"gopkg.in/ldap.v2"
)

type configurations struct {
	domain          string
	serviceAccount  string
	env             string
	servicePassword string
	searchDomain    string
	privateKey      string
	publicKey       string
}

// newServiceLDAPConn creates a new connection with the specified configuration.
// The caller is responsible for closing the connection when finished.
func newServiceLDAPConn(c configurations) (*ldap.Conn, error) {
	ldapConn, err := ldap.Dial("tcp", fmt.Sprintf("%s:%d", c.domain, 389))
	if err != nil {
		return nil, err
	}
	err = ldapConn.Bind(c.serviceAccount, c.servicePassword)
	if err != nil {
		ldapConn.Close()
		return nil, err
	}
	return ldapConn, nil
}

// getUserAttributes looks up displayName, title, mail, and telephoneNumber for
// a user.
func getUserAttributes(ldapConn *ldap.Conn, c configurations, user string) (string, map[string]string, error) {
	searchRequest := ldap.NewSearchRequest(
		c.searchDomain,
		ldap.ScopeWholeSubtree, ldap.NeverDerefAliases, 0, 0, false,
		"(&(objectCategory=person)(objectClass=user)(sAMAccountName="+user+"))",
		[]string{"dn", "cn", "displayName", "mail", "title", "telephoneNumber"},
		nil,
	)
	attrs := make(map[string]string)
	sr, err := ldapConn.Search(searchRequest)
	if err != nil {
		log.Error(err)
		return "", attrs, err
	}
	if len(sr.Entries) != 1 {
		log.Warnf("Username password combination for %v was invalid", user)
		return "", attrs, nil
	}
	targetOU := ""
	for _, entry := range sr.Entries {
		targetOU = entry.DN
		attrs["displayName"] = entry.GetAttributeValue("displayName")
		attrs["title"] = entry.GetAttributeValue("title")
		attrs["mail"] = entry.GetAttributeValue("mail")
		attrs["telephoneNumber"] = entry.GetAttributeValue("telephoneNumber")
	}
	return targetOU, attrs, nil
}

// getUserGroups gets all groups to which a user belongs, including nested groups.
//
// As an example of why this is useful, the one-loan-dashboard-user group in GRI
// is a member of One-Loan-Read. If a user is explicitly in the
// one-loan-dashboard-user group but not the One-Loan-Read group, this function
// will return the user as a member of both groups.
func getUserGroups(ldapConn *ldap.Conn, c configurations, dn string) ([]string, error) {
	// The implementation uses LDAP_MATCHING_RULE_IN_CHAIN as part of the
	// lookup, ensuring that the query will pull nested groups. If you need to
	// change this function to exclude nested groups, you should use the query:
	// "(member:="+dn+")".
	//
	// Reference:
	// https://ldapwiki.com/wiki/Active%20Directory%20User%20Related%20Searches#section-Active+Directory+User+Related+Searches-AllGroupsAUserIsAMemberOfIncludingNestedGroups
	searchRequest := ldap.NewSearchRequest(
		c.searchDomain,
		ldap.ScopeWholeSubtree, ldap.NeverDerefAliases, 0, 0, false,
		"(member:1.2.840.113556.1.4.1941:="+dn+")",
		[]string{"memberOf"},
		nil,
	)
	groups := make([]string, 0)
	sr, err := ldapConn.Search(searchRequest)
	if err != nil {
		log.Error(err)
		return groups, err
	}
	if len(sr.Entries) == 0 {
		log.Warnf("User %v not found", dn)
		return groups, nil
	}
	for _, group := range sr.Entries {
		parsed := strings.Split(group.DN, ",")
		g := ""
		for _, i := range parsed {
			if strings.Contains(i, "CN=") {
				small := strings.Replace(i, "CN=", "", -1)
				g = strings.TrimSpace(small)
			}
		}
		if g != "" {
			groups = append(groups, g)
		}
	}
	return groups, nil
}

// getUser looks up a user and returns the target OU, the list of groups to
// which the member belongs, and the user attributes.
func getUser(ldapConn *ldap.Conn, ctx context.Context, c configurations, user string) (string, []string, map[string]string, error) {
	targetOU, attrs, err := getUserAttributes(ldapConn, currentConfig, user)
	if err != nil || targetOU == "" {
		return targetOU, []string{}, attrs, err
	}
	groups, err := getUserGroups(ldapConn, currentConfig, targetOU)
	// cache the user
	return targetOU, groups, attrs, err
}

func verifyUserCredentials(ctx context.Context, c configurations, targetOU string, user string, password string) (bool, error) {
	userConn, err := ldap.Dial("tcp", fmt.Sprintf("%s:%d", c.domain, 389))
	if err != nil {
		log.Error(err.Error())
		return false, errors.New("We had an issue contacting the domain to check the user credentials.")
	}
	defer userConn.Close()
	err = userConn.Bind(targetOU, password)
	if err != nil {
		log.Info("ERROR on BIND")
		log.Warn(err.Error())
		log.Warnf("Username password combination for %v was invalid", user)
		return false, nil
	}
	return true, nil
}

func verifyUser(ldapConn *ldap.Conn, ctx context.Context, user string, password string) (bool, []string, map[string]string, error) {
	targetOU, groups, attrs, err := getUser(ldapConn, ctx, currentConfig, user)
	if err != nil || targetOU == "" {
		return false, groups, attrs, err
	}
	authed, err := verifyUserCredentials(ctx, currentConfig, targetOU, user, password)
	return authed, groups, attrs, err
}

var currentConfig configurations = configurations{
	domain:          "lag13corp.lag13rate.ad",
	serviceAccount:  "cn=cool,ou=Service Accounts,ou=CB Cool Accounts,dc=lag13corp,dc=lag13rate,dc=ad",
	searchDomain:    "dc=lag13corp,dc=lag13rate,dc=ad",
	servicePassword: os.Getenv("SERVICE_AD_PASSWORD"),
}

func BenchmarkLDAPConn(b *testing.B) {
	for i := 0; i < b.N; i++ {
		if _, err := newServiceLDAPConn(currentConfig); err != nil {
			panic(err)
		}
	}
}

func BenchmarkVerifyUser(b *testing.B) {
	ldapConn, err := newServiceLDAPConn(currentConfig)
	if err != nil {
		panic(err)
	}
	defer ldapConn.Close()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, _, _, err := verifyUser(ldapConn, context.Background(), "lgroenendaal", os.Getenv("AD_PASSWORD")); err != nil {
			panic(err)
		}
	}
}

func BenchmarkGetUser(b *testing.B) {
	ldapConn, err := newServiceLDAPConn(currentConfig)
	if err != nil {
		panic(err)
	}
	defer ldapConn.Close()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, _, _, err := getUser(ldapConn, context.Background(), currentConfig, "lgroenendaal"); err != nil {
			panic(err)
		}
	}
}

func BenchmarkGetUserAttributes(b *testing.B) {
	ldapConn, err := newServiceLDAPConn(currentConfig)
	if err != nil {
		panic(err)
	}
	defer ldapConn.Close()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, _, err := getUserAttributes(ldapConn, currentConfig, "lgroenendaal"); err != nil {
			panic(err)
		}
	}
}

func BenchmarkGetUserGroups(b *testing.B) {
	ldapConn, err := newServiceLDAPConn(currentConfig)
	if err != nil {
		panic(err)
	}
	defer ldapConn.Close()
	targetOU, _, err := getUserAttributes(ldapConn, currentConfig, "lgroenendaal")
	if err != nil {
		panic(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := getUserGroups(ldapConn, currentConfig, targetOU); err != nil {
			panic(err)
		}
	}
}

func BenchmarkVerifyUserCreds(b *testing.B) {
	ldapConn, err := newServiceLDAPConn(currentConfig)
	if err != nil {
		panic(err)
	}
	defer ldapConn.Close()
	targetOU, _, _, err := getUser(ldapConn, context.Background(), currentConfig, "lgroenendaal")
	if err != nil {
		panic(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := verifyUserCredentials(context.Background(), currentConfig, targetOU, "lgroenendaal", os.Getenv("AD_PASSWORD")); err != nil {
			panic(err)
		}
	}
}
