GNATdoc.Documentation = {
  "label": "Bullfrog.Access_Types.Reference_Counts",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Basic_Count",
          "qualifier": "",
          "line": 36,
          "column": 9,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 36,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
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
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "is"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "mod"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "2"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "**"
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "32"
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
                  "text": "Basic count type for reference counting\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Simple types"
    },
    {
      "entities": [
        {
          "label": "Reference_Count",
          "qualifier": "",
          "line": 45,
          "column": 9,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 45,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Atomic",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L45C25"
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
                      "cssClass": "identifier",
                      "text": "Boolean"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "is"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "limited"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "private"
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
                  "text": "Main reference count type of the package\n"
                }
              ]
            }
          ],
          "fields": [
            {
              "label": "Atomic",
              "line": 45,
              "column": 25,
              "type": {
                "label": "Boolean"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Record types"
    },
    {
      "entities": [
        {
          "label": "Atomic_Reference_Count",
          "qualifier": "",
          "line": 81,
          "column": 19,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "href": "../docs/bullfrog__access_types__reference_counts___atomic_reference_count___spec.html#L81C19"
        }
      ],
      "label": "Protected objects and protected types"
    },
    {
      "entities": [
        {
          "label": "Decrement",
          "qualifier": "",
          "line": 67,
          "column": 14,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 67,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L67C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L67C24"
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
                      "text": "in"
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
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 68,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
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
          ],
          "parameters": [
            {
              "label": "Count",
              "line": 67,
              "column": 24,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get",
          "qualifier": "",
          "line": 48,
          "column": 13,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 48,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L48C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L48C17"
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
                      "cssClass": "identifier",
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 49,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
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
          ],
          "parameters": [
            {
              "label": "Count",
              "line": 48,
              "column": 17,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Increment",
          "qualifier": "",
          "line": 53,
          "column": 14,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 53,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L53C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L53C24"
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
                      "text": "in"
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
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 54,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
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
          ],
          "parameters": [
            {
              "label": "Count",
              "line": 53,
              "column": 24,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Post_Decrement",
          "qualifier": "",
          "line": 71,
          "column": 13,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 71,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "text": "Post_Decrement",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L71C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L71C28"
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
                      "text": "in"
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
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 72,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
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
              "label": "Count",
              "line": 71,
              "column": 28,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Post_Increment",
          "qualifier": "",
          "line": 58,
          "column": 13,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 58,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "text": "Post_Increment",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L58C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L58C28"
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
                      "text": "in"
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
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 59,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
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
              "label": "Count",
              "line": 58,
              "column": 28,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Pre_Decrement",
          "qualifier": "",
          "line": 75,
          "column": 13,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 75,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "text": "Pre_Decrement",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L75C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L75C27"
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
                      "text": "in"
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
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 76,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
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
              "label": "Count",
              "line": 75,
              "column": 27,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Pre_Increment",
          "qualifier": "",
          "line": 63,
          "column": 13,
          "src": "srcs/bullfrog-access_types-reference_counts.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 63,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "text": "Pre_Increment",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L63C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L63C27"
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
                      "text": "in"
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
                      "text": "Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 64,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
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
              "label": "Count",
              "line": 63,
              "column": 27,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
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