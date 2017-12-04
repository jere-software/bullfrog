GNATdoc.Documentation = {
  "label": "Bullfrog.Access_Types.Reference_Counts.Atomic_Reference_Count",
  "qualifier": "",
  "summary": [
  ],
  "description": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "Protected implementation provides atomic access to the count\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Decrement",
          "qualifier": "",
          "line": 99,
          "column": 17,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 99,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Decrement",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L99C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Decrement the value by 1\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Get",
          "qualifier": "",
          "line": 84,
          "column": 16,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 84,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Get",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L84C16"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Basic_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Gets the current value of the reference count\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Increment",
          "qualifier": "",
          "line": 88,
          "column": 17,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 88,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Increment",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L88C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Increment the value by 1\n"
                },
                {
                  "kind": "span",
                  "text": "Does not increment if the value is Basic_Count'First\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Post_Decrement",
          "qualifier": "",
          "line": 102,
          "column": 17,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 102,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Post_Decrement",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L102C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L102C32"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Basic_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Decrement the value by 1 and return the original value\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Value",
              "line": 102,
              "column": 32,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Basic_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Post_Increment",
          "qualifier": "",
          "line": 92,
          "column": 17,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 92,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Post_Increment",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L92C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L92C32"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Basic_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Increment the value by 1 and return the original value\n"
                },
                {
                  "kind": "span",
                  "text": "Does not increment if the value is Basic_Count'First\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Value",
              "line": 92,
              "column": 32,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Basic_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Pre_Decrement",
          "qualifier": "",
          "line": 105,
          "column": 17,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 105,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Pre_Decrement",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L105C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L105C31"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Basic_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Decrement the value by 1 and return the final value\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Value",
              "line": 105,
              "column": 31,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Basic_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Pre_Increment",
          "qualifier": "",
          "line": 96,
          "column": 17,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 96,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Pre_Increment",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L96C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L96C31"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Basic_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Increment the value by 1 and return the final value\n"
                },
                {
                  "kind": "span",
                  "text": "Does not increment if the value is Basic_Count'First\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Value",
              "line": 96,
              "column": 31,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Basic_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L36C9"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    }
  ]
};