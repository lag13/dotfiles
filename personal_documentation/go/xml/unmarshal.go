// After playing around more with XML's marshalling I felt more
// comfortable with the underlying algorithm and wanted to try my hand
// at unmarshalling some XML I encountered at work. I'm happy to say
// that I felt much more comfortable doing it this time around and
// even though I ended up producing more or less the same XML I feel
// better about the naming and such.
package main

import (
	"encoding/xml"
	"fmt"
)

const xmlStr = `<?xml version="1.0" encoding="UTF-8"?>
<root>
 <result>
  <Prop0>
   <type>num</type>
   <label>
    <en>Candidate ID</en>
    <fr>Référence du candidat</fr>
   </label>
  </Prop0>
  <Prop1>
   <type>text</type>
   <label>
    <en>First Name</en>
    <fr>Prenom</fr>
   </label>
  </Prop1>
  <Prop2>
   <type>text</type>
   <label>
    <en>Last Name</en>
    <fr>Nom</fr>
   </label>
  </Prop2>
 </result>
</root>`

type XMLSchema struct {
	XMLName xml.Name `xml:"root"`
	Result  Result   `xml:"result"`
}

type Result struct {
	Props []Prop `xml:",any"`
}

type Prop struct {
	XMLName xml.Name
	Type    string `xml:"type"`
	Label   Label  `xml:"label"`
}

type Label struct {
	LanguageLabel []LanguageLabel `xml:",any"`
}

type LanguageLabel struct {
	XMLName xml.Name
	Value   string `xml:",innerxml"`
}

func main() {
	schema := XMLSchema{}
	if err := xml.Unmarshal([]byte(xmlStr), &schema); err != nil {
		panic(err)
	}
	fmt.Printf("%+v\n", schema)
}
