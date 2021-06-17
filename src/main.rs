use jlc;

fn main() {
    let expression = jlc::arithmetic::expression(r#"
        if ( 
            payload.v.0.tg === external.acceptance-criterias.diseases.sarscov2 ?? "test"
            && payload.v.0.dn >= payload.v.0.sd 
            && payload.v.0.sd >= 2
            && payload.v.0.dn == 2
        )
        {
            true
        } else
        { 
            false
        }
    "#).unwrap();
    
    let expression = expression.to_json_logic();
   
    println!("{}", serde_json::to_string_pretty(&expression).unwrap());
    let data  = serde_json::json!(
        {
            "external": {
                "acceptance-criterias" : {
                    "diseases" : {
                        "sarscov2" : "840539006"
                    }
                }
            },
            "payload" : {
            "nam": {
                "fn": "Müller",
                "fnt": "MUELLER",
                "gn": "Céline",
                "gnt": "CELINE"
            },
            "dob": "1943-02-01",
            "ver": "1.0.0",
            "v": [
                {
                    "tg": "840539006",
                    "vp": "1119349007",
                    "mp": "EU/1/20/1507",
                    "ma": "ORG-100031184",
                    "dn": 2,
                    "sd": 2,
                    "dt": "2021-04-30",
                    "co": "CH",
                    "is": "Bundesamt für Gesundheit (BAG)",
                    "ci": "urn:uvci:01:CH:2987CC9617DD5593806D4285"
                }
            ]
        }
    });
    println!("{:#?}", jsonlogic::apply(&expression, &data));
}
