# Jsonlogic-Compiler

This repository provides means of compiling a simple "javascript-like" language to [json-logic](https://jsonlogic.com/). We started this project to simplify writing of [CertLogic](https://github.com/eu-digital-green-certificates/dgc-certlogic-android) rules for the verification of [GreenCertificates](https://github.com/eu-digital-green-certificates/dgc-overview). Since reading and reviewing json-logic gets quite tedious when the rule complexity increases, something easier to read was needed.

## AifC (AST is for Computers)

AifC /ɪˈfsiː/ [eff-see] is an acronym explaining the pain of json-logic. Json-logic itself is nice to use, since one can easily evaluate certain logic given a context in a secure sandbox. Though the syntax itself is more similiar to something like an AST. Hence, the layer above json-logic, providing a syntax more like modern programming languages.

## The compiler

There is a project in [aifc](./aifc), which provides a simple compiler backend. It takes an input file and produces the corresponding json file.

## The parser

The parser uses [peg](https://github.com/kevinmehall/rust-peg) to generate an internal AST from which the resulting json-logic is produced. The grammar rules can be found in [here](./src/parser.rs).

## The Web-Backend

To provide easy access to the language, we built a static website, using the rust parser as a web assembly module. The website should run on most modern browsers. It has built-in the monaco editor backend with a basic language definition for aifc.

On the web backend, the resulting rules can also be verified by evaluating them on a an actual JSON data context.

There is a [web-backend](http://jsonlogic-compiler.s3-website.eu-central-1.amazonaws.com/) available for testing.

## The Syntax Highlighting

To provide an easier development cycle, there exists a vscode language definition, providing basic syntax highlighting.


# The Language

> Note: The compiler automatically applies certain `CertLogic` specific desugaring

## Conditional Statements

### If-Statement

```js
if (a < b) { a }
```
<details>
<summary>JSON-logic</summary>

```js
{
  "if": [
    {
      "<": [
        {
          "var": "a"
        },
        {
          "var": "b"
        }
      ]
    },
    {
      "var": "a"
    }
  ]
}
```
</details>

### If-Else-Statement

```js
if (a < b) { a } else { b }
```

<details>
<summary>JSON-logic</summary>

```json
{
  "if": [
    {
      "<": [
        {
          "var": "a"
        },
        {
          "var": "b"
        }
      ]
    },
    {
      "var": "a"
    },
    {
      "var": "b"
    }
  ]
}
```
</details>

### Switch statement

The switch statement allows easy switching on multiple conditions. Further, one can define sub conditions after the switch case to allow for certain pattern matching capabilities. One should note that providing an array in the case clause, will use the `in` opperator, to check if the switch clause is contained in the case clause.

> Every switch statement needs a default case!

```js
switch(medical_product) {
    ["a", "b"] : if dosis_number === 1 => {
        true
    }
    "c" => {
        true
    }
    _ => { 
        false
    }
}
```

<details>
<summary>JSON-logic</summary>

```json
{
  "if": [
    {
      "and": [
        {
          "in": [
            {
              "var": "medical_product"
            },
            [
              "a",
              "b"
            ]
          ]
        },
        {
          "===": [
            {
              "var": "dosis_number"
            },
            1
          ]
        }
      ]
    },
    true,
    {
      "if": [
        {
          "===": [
            {
              "var": "medical_product"
            },
            "c"
          ]
        },
        true,
        false
      ]
    }
  ]
}
```
</details>

### Logical operators 

The following logic operators are supported:

- and / &&
- or / ||
- not / !

```js
a and b
```

<details>
<summary>JSON-logic</summary>

```json
{
  "and": [
    {
      "var": "a"
    },
    {
      "var": "b"
    }
  ]
}
```
</details>

```js
a or b
```

<details>
<summary>JSON-logic</summary>

```json
{
  "!": [
    {
      "and": [
        {
          "!": [
            {
              "var": "a"
            }
          ]
        },
        {
          "!": [
            {
              "var": "b"
            }
          ]
        }
      ]
    }
  ]
}
```
</details>

```js
not b
```

<details>
<summary>JSON-logic</summary>

```json
{
  "!": [
    {
      "var": "b"
    }
  ]
}
```
</details>

### Time-Comparison

When using comparison of time events the following operators _must_ be used to comply with `CertLogic`:

- is before
- is not before
- is after
- is not after

```js
a as DateTime is before b as DateTime
```

<details>
<summary>JSON-logic</summary>

```json
{
  "before": [
    {
      "var": "a"
    },
    {
      "var": "b"
    }
  ]
}
```
</details>

### Time-Literals

Suffixing a integer literal with `#<timeunit>` will automatically desugar to the corresponding `CertLogic` `plusTime` logic, if applied to a `date`. Note since time operations are only supported on time variables, an `as DateTime` cast is needed.

> Note that all the time operations are not well defined on weather what a month is. The web-backend uses `chrono` as the time library and hence all operations are lowered to the corresponding `chrono` functions.

- hour/hours
- day/days
- month/months
- year/years

```js 
a as DateTime + 1#day
```
<details>
<summary>JSON-logic</summary>

```js
{
  "plusTime": [
    {
      "var": "a"
    },
    1,
    "day"
  ]
}
```
</details>

Further, strings conforming to a date or date time are automatically considered as `date variables`:

```js
"2021-01-01"
```

<details>
<summary>JSON-logic</summary>

```js
{
  "plusTime": [
    "2021-01-01",
    0,
    "day"
  ]
}
```
</details>

To be able to use the date-string in a time-operation, a cast to string is needed, as CertLogic can only operate on strings:
```js
("2020-01-01" as String) + 1#day
```


<details>
<summary>JSON-logic</summary>

```js
{
  "plusTime": [
    "2021-01-01",
    1,
    "day"
  ]
}
```
</details>

### Time Functions

For time evaluation the shorthand `now()` exists, which desugars to a variable from the context.

```js
now()
```

<details>
<summary>JSON-logic</summary>

```js
{
  "var": "external.validationClock"
}
```
</details>

### Boolean casts

As `CertLogic` only provides a subset of `truthy/falsy` values we added the `Boolean` cast to ensure something is interpreted as a boolean.

```js
a as Boolean
```

<details>
<summary>JSON-logic</summary>

```js
{
  "!": [
    {
      "!": [
        {
          "var": "a"
        }
      ]
    }
  ]
}
```
</details>


### Built in desugared functions

For the `min` and `max` expression desugarings are available:

```js
min(a,b,c,d)
```

<details>
<summary>JSON-logic</summary>

```js
{
  "if": [
    {
      "<": [
        {
          "if": [
            {
              "<": [
                {
                  "if": [
                    {
                      "<": [
                        {
                          "if": [
                            {
                              "<": [
                                {
                                  "if": [
                                    {
                                      "<": [
                                        {
                                          "var": "a"
                                        },
                                        {
                                          "var": "b"
                                        }
                                      ]
                                    },
                                    {
                                      "var": "a"
                                    },
                                    {
                                      "var": "b"
                                    }
                                  ]
                                },
                                {
                                  "var": "a"
                                }
                              ]
                            },
                            {
                              "if": [
                                {
                                  "<": [
                                    {
                                      "var": "a"
                                    },
                                    {
                                      "var": "b"
                                    }
                                  ]
                                },
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            }
                          ]
                        },
                        {
                          "var": "b"
                        }
                      ]
                    },
                    {
                      "if": [
                        {
                          "<": [
                            {
                              "if": [
                                {
                                  "<": [
                                    {
                                      "var": "a"
                                    },
                                    {
                                      "var": "b"
                                    }
                                  ]
                                },
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            }
                          ]
                        },
                        {
                          "if": [
                            {
                              "<": [
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        }
                      ]
                    },
                    {
                      "var": "b"
                    }
                  ]
                },
                {
                  "var": "c"
                }
              ]
            },
            {
              "if": [
                {
                  "<": [
                    {
                      "if": [
                        {
                          "<": [
                            {
                              "if": [
                                {
                                  "<": [
                                    {
                                      "var": "a"
                                    },
                                    {
                                      "var": "b"
                                    }
                                  ]
                                },
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            }
                          ]
                        },
                        {
                          "if": [
                            {
                              "<": [
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        }
                      ]
                    },
                    {
                      "var": "b"
                    }
                  ]
                },
                {
                  "if": [
                    {
                      "<": [
                        {
                          "if": [
                            {
                              "<": [
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        }
                      ]
                    },
                    {
                      "if": [
                        {
                          "<": [
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        },
                        {
                          "var": "b"
                        }
                      ]
                    },
                    {
                      "var": "a"
                    }
                  ]
                },
                {
                  "var": "b"
                }
              ]
            },
            {
              "var": "c"
            }
          ]
        },
        {
          "var": "d"
        }
      ]
    },
    {
      "if": [
        {
          "<": [
            {
              "if": [
                {
                  "<": [
                    {
                      "if": [
                        {
                          "<": [
                            {
                              "if": [
                                {
                                  "<": [
                                    {
                                      "var": "a"
                                    },
                                    {
                                      "var": "b"
                                    }
                                  ]
                                },
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            }
                          ]
                        },
                        {
                          "if": [
                            {
                              "<": [
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        }
                      ]
                    },
                    {
                      "var": "b"
                    }
                  ]
                },
                {
                  "if": [
                    {
                      "<": [
                        {
                          "if": [
                            {
                              "<": [
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        }
                      ]
                    },
                    {
                      "if": [
                        {
                          "<": [
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        },
                        {
                          "var": "b"
                        }
                      ]
                    },
                    {
                      "var": "a"
                    }
                  ]
                },
                {
                  "var": "b"
                }
              ]
            },
            {
              "var": "c"
            }
          ]
        },
        {
          "if": [
            {
              "<": [
                {
                  "if": [
                    {
                      "<": [
                        {
                          "if": [
                            {
                              "<": [
                                {
                                  "var": "a"
                                },
                                {
                                  "var": "b"
                                }
                              ]
                            },
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        }
                      ]
                    },
                    {
                      "if": [
                        {
                          "<": [
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        },
                        {
                          "var": "b"
                        }
                      ]
                    },
                    {
                      "var": "a"
                    }
                  ]
                },
                {
                  "var": "b"
                }
              ]
            },
            {
              "if": [
                {
                  "<": [
                    {
                      "if": [
                        {
                          "<": [
                            {
                              "var": "a"
                            },
                            {
                              "var": "b"
                            }
                          ]
                        },
                        {
                          "var": "a"
                        },
                        {
                          "var": "b"
                        }
                      ]
                    },
                    {
                      "var": "a"
                    }
                  ]
                },
                {
                  "if": [
                    {
                      "<": [
                        {
                          "var": "a"
                        },
                        {
                          "var": "b"
                        }
                      ]
                    },
                    {
                      "var": "a"
                    },
                    {
                      "var": "b"
                    }
                  ]
                },
                {
                  "var": "a"
                }
              ]
            },
            {
              "var": "b"
            }
          ]
        },
        {
          "var": "c"
        }
      ]
    },
    {
      "var": "d"
    }
  ]
}
```
</details>

```js
max(a,b)
```

<details>
<summary>JSON-logic</summary>

```js
{
  "if": [
    {
      ">": [
        {
          "var": "a"
        },
        {
          "var": "b"
        }
      ]
    },
    {
      "var": "a"
    },
    {
      "var": "b"
    }
  ]
}
```
</details>


### Comments

There are only multiline comments supported, using the following syntax

```js
/* This is a single line */

/*
    This is a multi line... 
    ...comment
*/
```

### Variable Assignments

Currently, basic variable statements are supported. Anything which either evaluates to a previously defined variable or an expression can be used as an rvalue. The compiler will report variables which are defined twice. 
  
Variables are scoped to the corresponding block, inheriting the parent scope. Consider the following examples:

```js
let a = b;
let c = a;
c
```

will produce

```json
{
  "var": "b"
}
```

The following will fail, since `a` is defined twice:

```js
let a = "test";
let a = 1;
```

This on the other hand is fine, since `a` is scoped to either the `if` or `else` branch.
```js
if (true) {
  let a = "true";
  a
} else {
  let a = "false";
  a
}
```
  
### Imports
The compiler will allow `import` statements to include other files. First all input is stiched together and then the compiler tries to parse it. As such, only imports which define variables via `let` binding are supported. For obvious reasons, the online web compiler does _not_ support import statements. Imported files __must__ be referenced relative to the file being compiled.
  
`globals.aifc`
```js
 let yes = true;
 let no = false;
```
`test.aifc`
```js
/* globals.aifc is in the same directory as test.aifc */
import "globals.aifc";

if (yes) {
  true
} else {
  false 
}
```
