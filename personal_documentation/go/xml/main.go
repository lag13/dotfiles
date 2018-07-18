package main

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
)

// https://stackoverflow.com/questions/10922269/unmarshal-dynamic-xml

// Schema represents the candidate XML schema returned from the legacy
// API.
type Schema struct {
	Props PropList `xml:"result"`
}

// PropList is me trying to deal with Go's XML parsing library.
// Because the legacy API returns a big list of
// <Prop:num:>...</Prop:num:> nodes and I suppose I could have a big
// struct for each one but that seems not worth the effort so I want
// to just parse them all as "Prop" types and record the name. The
// ",any" flag is what does that, it means that any XML nodes on this
// level are parsed.
type PropList struct {
	Props []Prop `xml:",any"`
}

// Prop is structured information about the PropID XML nodes returned
// from the API.
type Prop struct {
	XMLName  xml.Name
	Type     string    `xml:"type"`
	Label    LabelList `xml:"label"`
	ListType int       `xml:"type-liste"`
	RefType  int       `xml:"type-noeud"`
}

// LabelList is like the PropList in function. See each field can have
// a label which is different depending on the language. But I am not
// sure what languages there are hence I have the ",any" here.
type LabelList struct {
	Labels []Label `xml:",any"`
}

// Label contains the XML name of the node and the text inside which
// is the actual label. An example is <en>Hello there</en>.
type Label struct {
	XMLName xml.Name
	Label   string `xml:",innerxml"`
}

func main() {
	candSchema, err := ioutil.ReadFile("./candSchema.xml")
	if err != nil {
		panic(err)
	}
	var schema Schema
	if err := xml.Unmarshal(candSchema, &schema); err != nil {
		panic(err)
	}
	for _, prop := range schema.Props.Props {
		fmt.Println(prop.XMLName.Local)
	}
}
