package main

import (
	"encoding/json"
	"strconv"
	"strings"
	"time"
)

// Always remember that *json.RawMessage implements the
// Marshaler interface, json.RawMessage does not.

// This is the most complicated Go JSON parsing I've done hence I'm
// keeping it here for future reference:

// UnixTimeMs represents a unix timestamp in milliseconds. TODO: This
// could be moved to a separate package and if that ever happens then
// we should probably:
// 1. Consider adding the ability to unmarshal strings OR ints. We
//    should research this first to see if APIs actually do this
//    though.
// 2. Add a MarshalJSON() method which will marshal the value as a
//    unix time stamp.
type UnixTimeMs struct {
	time.Time
}

// UnmarshalJSON converts JSON to a time.Time value.
func (u *UnixTimeMs) UnmarshalJSON(data []byte) error {
	ms, err := strconv.ParseInt(string(data), 10, 64)
	if err != nil {
		return err
	}
	sec := ms / 1000
	msWithoutSec := ms % 1000
	ns := msWithoutSec * 1000 * 1000
	u.Time = time.Unix(sec, ns)
	return nil
}

// StrInt represents a string that contains an integer.
type StrInt string

// UnmarshalJSON converts JSON to a StrInt. It will convert both the
// JSON string "12" and the JSON number 12 to the Go string 12. The
// reason this exists is because on the snooze/snoozed_systems
// endpoint we have seen a customer system ID returned as a number OR
// as a string and unmarshalling in this fashion will result in the
// same value. If the API enforces one format or another then this
// logic can be removed.
func (s *StrInt) UnmarshalJSON(data []byte) error {
	v := strings.Replace(string(data), `"`, ``, -1)
	if _, err := strconv.ParseInt(v, 10, 64); err != nil {
		return err
	}
	*s = StrInt(v)
	return nil
}

// SnoozedSystem represents a system returned by the ATS
// snoozed-systems API.
type SnoozedSystem struct {
	ID        string
	Timestamp time.Time
}

// UnmarshalJSON converts JSON to a SnoozedSystem.
func (s *SnoozedSystem) UnmarshalJSON(data []byte) error {
	var snoozedSystem struct {
		System    StrInt     `json:"system"`
		Timestamp UnixTimeMs `json:"timestamp"`
	}
	if err := json.Unmarshal(data, &snoozedSystem); err != nil {
		return err
	}
	s.ID = string(snoozedSystem.System)
	s.Timestamp = snoozedSystem.Timestamp.Time
	return nil
}
