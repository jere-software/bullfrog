GNATdoc.Documentation = {
  "label": "Bullfrog.Access_Types.Smart_Access",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Atomic_Increment",
          "qualifier": "",
          "line": 60,
          "column": 4,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 60,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Atomic_Increment"
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
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":="
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "True"
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
                  "text": "This specifies whether or not to use atomic increment (a count wrapped\n"
                },
                {
                  "kind": "span",
                  "text": "in a protected object).  For single task applications, this should\n"
                },
                {
                  "kind": "span",
                  "text": "be False.  When True, it does not guarantee task saftey on the\n"
                },
                {
                  "kind": "span",
                  "text": "Smart_Access types, but it does guarantee that seperate Smart_Access\n"
                },
                {
                  "kind": "span",
                  "text": "variables in separate tasks can safely manage the same resource.  If one\n"
                },
                {
                  "kind": "span",
                  "text": "needs multiple tasks to access the same Smart_Access variable, however,\n"
                },
                {
                  "kind": "span",
                  "text": "it will need to be wrapped in some sort of synchronization primitive.\n"
                },
                {
                  "kind": "span",
                  "text": "It also does not guarantee task safety on the resource itself.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Finalize",
          "qualifier": "",
          "line": 50,
          "column": 19,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 50,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
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
                      "text": "Finalize",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L50C19"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Memory",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L50C28"
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
                      "text": "Item_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L45C9"
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
                  "text": "A finalization procedure to be called when all\n"
                },
                {
                  "kind": "span",
                  "text": "refernces to the Item_Access variable are gone.\n"
                },
                {
                  "kind": "span",
                  "text": "The client should never manually call this procedure.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Item_Access",
          "qualifier": "",
          "line": 45,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
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
                      "text": "Item_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L45C9"
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
                      "text": "access"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Item_Type"
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
                  "text": "The desired named access type that pairs with Item_Type\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Item_Type",
          "qualifier": "",
          "line": 42,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 42,
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
                      "text": "Item_Type",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L42C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "<>"
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
                  "text": "The basic type held by a Smart_Access type\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Memory",
          "qualifier": "",
          "line": 50,
          "column": 28,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
          ]
        }
      ],
      "label": "Generic formals"
    },
    {
      "entities": [
        {
          "label": "Basic_Count",
          "qualifier": "",
          "line": 75,
          "column": 12,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
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
                      "text": "subtype"
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
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L75C12"
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
                      "cssClass": "identifier",
                      "text": "Bullfrog.Access_Types.Reference_Counts.Basic_Count",
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
                  "text": "Basic counting type for the underlying reference count\n"
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
          "label": "Constant_Item_Access",
          "qualifier": "",
          "line": 71,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
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
                      "text": "Constant_Item_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L71C9"
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
                      "text": "access"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "constant"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Item_Type"
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
                  "text": "This type provides read only access to the resource\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Counts_Access",
          "qualifier": "",
          "line": 372,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 372,
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
                      "text": "Counts_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L372C9"
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
                      "text": "access"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Counts",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L366C9"
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
                  "text": "Access type for the counts container\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Access types"
    },
    {
      "entities": [
        {
          "label": "Counts",
          "qualifier": "",
          "line": 366,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 366,
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
                      "text": "Counts",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L366C9"
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
                      "text": "record"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 367,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Strong",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L367C7"
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
                      "text": "Reference_Counts.Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Atomic_Increment"
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
                },
                {
                  "kind": "line",
                  "number": 368,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Weak",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L368C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "text": "Reference_Counts.Reference_Count",
                      "href": "docs/bullfrog__access_types__reference_counts___spec.html#L115C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Atomic_Increment"
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
                },
                {
                  "kind": "line",
                  "number": 369,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "end"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "record"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L366C9"
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
                  "text": "Container for the two reference counts\n"
                }
              ]
            }
          ],
          "fields": [
            {
              "label": "Strong",
              "line": 367,
              "column": 7,
              "type": {
                "label": "Bullfrog.Access_Types.Reference_Counts.Reference_Count",
                "docHref": "docs/bullfrog__access_types__reference_counts___spec.html#L45C9"
              },
              "description": [
              ]
            },
            {
              "label": "Weak",
              "line": 368,
              "column": 7,
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
      "label": "Record types"
    },
    {
      "entities": [
        {
          "label": "Shared_Access",
          "qualifier": "",
          "line": 81,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 81,
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
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
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
                      "text": "new"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Ada.Finalization.Controlled"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
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
                  "text": "This Smart_Access type provides reference counting semantics.  It can\n"
                },
                {
                  "kind": "span",
                  "text": "be copied and will automatically handle deallocating the held access\n"
                },
                {
                  "kind": "span",
                  "text": "variable.  Do not create circular references with this type.  Instead\n"
                },
                {
                  "kind": "span",
                  "text": "use Weak_Access types to break any circular connections.\n"
                }
              ]
            }
          ],
          "inherits": [
            {
              "label": "Controlled",
              "docHref": "docs/controlled___spec.html#L43C9"
            }
          ],
          "inherited": [
            {
              "label": "Shared_Access",
              "docHref": "docs/bullfrog__access_types__simple_smart_access___spec.html#L76C12"
            }
          ]
        },
        {
          "label": "Unique_Access",
          "qualifier": "",
          "line": 94,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 94,
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
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
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
                      "text": "new"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Ada.Finalization.Limited_Controlled"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
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
                  "text": "This Smart_Access type is the \"goto\" access manager for most situations.\n"
                },
                {
                  "kind": "span",
                  "text": "Only one Unique_Access variable can hold a reference at a time.\n"
                },
                {
                  "kind": "span",
                  "text": "Ownership can be transferred to other Unique_Access variables when\n"
                },
                {
                  "kind": "span",
                  "text": "desired.\n"
                }
              ]
            }
          ],
          "inherits": [
            {
              "label": "Limited_Controlled",
              "docHref": "docs/limited_controlled___spec.html#L50C9"
            }
          ],
          "inherited": [
            {
              "label": "Unique_Access",
              "docHref": "docs/bullfrog__access_types__simple_smart_access___spec.html#L82C12"
            }
          ]
        },
        {
          "label": "Weak_Access",
          "qualifier": "",
          "line": 88,
          "column": 9,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
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
                      "text": "Weak_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
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
                      "text": "new"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Ada.Finalization.Controlled"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
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
                  "text": "This Smart_Access type provides minimal utility and mainly exists to\n"
                },
                {
                  "kind": "span",
                  "text": "help manage circular references.  If a Weak_Access variable exists\n"
                },
                {
                  "kind": "span",
                  "text": "and points to the same object as a Shared_Access type, then the\n"
                },
                {
                  "kind": "span",
                  "text": "reference count object associated with them will not finalize until\n"
                },
                {
                  "kind": "span",
                  "text": "all Weak_Access types and Shared_Access types have finalized.\n"
                }
              ]
            }
          ],
          "inherits": [
            {
              "label": "Controlled",
              "docHref": "docs/controlled___spec.html#L43C9"
            }
          ]
        }
      ],
      "label": "Tagged types"
    },
    {
      "entities": [
        {
          "label": "=",
          "qualifier": "",
          "line": 145,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 145,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "overriding"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 146,
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
                      "text": "\"=\"",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L146C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 147,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right"
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
                      "text": "Shared_Access"
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
                  "number": 148,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 149,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 150,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 146,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 146,
              "column": 13,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "=",
          "qualifier": "",
          "line": 192,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 192,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "overriding"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 193,
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
                      "text": "\"=\"",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L193C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 194,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right"
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
                      "text": "Weak_Access"
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
                  "number": 195,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 196,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 197,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 193,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 193,
              "column": 13,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Adjust",
          "qualifier": "",
          "line": 172,
          "column": 25,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 172,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "overriding"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                      "text": "Adjust",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L172C25"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L172C34"
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
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
                  "text": "Override for Ada.Finalization.Controlled\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 172,
              "column": 34,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Adjust",
          "qualifier": "",
          "line": 225,
          "column": 25,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 225,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "overriding"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                      "text": "Adjust",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L225C25"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L225C34"
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
                      "text": "Weak_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L391C9"
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
                  "text": "Override for Ada.Finalization.Controlled\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 225,
              "column": 34,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Constant_Reference",
          "qualifier": "",
          "line": 137,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 137,
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
                      "text": "Constant_Reference",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L137C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 138,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L138C8"
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
                      "cssClass": "identifier",
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
                  "number": 139,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Constant_Item_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L71C9"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 140,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 141,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns a dereferenced read-only view of the Share_Access object's\n"
                },
                {
                  "kind": "span",
                  "text": "reference\n"
                },
                {
                  "kind": "span",
                  "text": "NOTE:  In the future (when GNAT is fixed) this will return a\n"
                }
              ]
            },
            {
              "kind": "code",
              "children": [
                {
                  "number": 1,
                  "children": [
                    {
                      "kind": "span",
                      "text": "variable of type References.Constant_Reference:"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "   package References is new Access_Types.References(Item_Type);"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "   function Constant_Reference"
                    }
                  ]
                },
                {
                  "number": 4,
                  "children": [
                    {
                      "kind": "span",
                      "text": "      (Self : in Shared_Access)"
                    }
                  ]
                },
                {
                  "number": 5,
                  "children": [
                    {
                      "kind": "span",
                      "text": "       return References.Constant_Reference;"
                    }
                  ]
                },
                {
                  "number": 6,
                  "children": [
                    {
                      "kind": "span",
                      "text": ""
                    }
                  ]
                },
                {
                  "number": 7,
                  "children": [
                    {
                      "kind": "span",
                      "text": "Then all calls of Object.Constant_Reference.all will need to be"
                    }
                  ]
                },
                {
                  "number": 8,
                  "children": [
                    {
                      "kind": "span",
                      "text": "replaced with just Object.Constant_Reference"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 138,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Constant_Reference",
          "qualifier": "",
          "line": 249,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 249,
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
                      "text": "Constant_Reference",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L249C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 250,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L250C8"
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
                      "cssClass": "identifier",
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L402C9"
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
                  "number": 251,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Constant_Item_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L71C9"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 252,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 253,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns a dereferenced read-only view of the Unique_Access object's\n"
                },
                {
                  "kind": "span",
                  "text": "reference\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 250,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Finalize",
          "qualifier": "",
          "line": 173,
          "column": 25,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 173,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "overriding"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                      "text": "Finalize",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L173C25"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L173C34"
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
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 173,
              "column": 34,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Finalize",
          "qualifier": "",
          "line": 226,
          "column": 25,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 226,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "overriding"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                      "text": "Finalize",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L226C25"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L226C34"
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
                      "text": "Weak_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L391C9"
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
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 226,
              "column": 34,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Finalize",
          "qualifier": "",
          "line": 273,
          "column": 25,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 273,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "overriding"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                      "text": "Finalize",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L273C25"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L273C34"
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
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L402C9"
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
                  "text": "Override for Ada.Finalization.Limited_Controlled\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 273,
              "column": 34,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Is_Null",
          "qualifier": "",
          "line": 152,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 152,
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
                      "text": "Is_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L152C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 153,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L153C8"
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
                      "cssClass": "identifier",
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
                  "number": 154,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 155,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 156,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns True if the object holds a null reference\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 153,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Is_Null",
          "qualifier": "",
          "line": 202,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 202,
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
                      "text": "Is_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L202C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 203,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L203C8"
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
                      "cssClass": "identifier",
                      "text": "Weak_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L391C9"
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
                  "number": 204,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 205,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 206,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns True if the object holds a null reference.  Take care if the\n"
                },
                {
                  "kind": "span",
                  "text": "result is False.  If a result of False is mandatory, consider creating\n"
                },
                {
                  "kind": "span",
                  "text": "a Shared_Access object from the Weak_Access object and check Is_Null\n"
                },
                {
                  "kind": "span",
                  "text": "with it.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 203,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Is_Null",
          "qualifier": "",
          "line": 256,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 256,
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
                      "text": "Is_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L256C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 257,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L257C8"
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
                      "cssClass": "identifier",
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L402C9"
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
                  "number": 258,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 259,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 260,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns True if the object holds a null reference\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 257,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Not_Null",
          "qualifier": "",
          "line": 159,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 159,
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
                      "text": "Not_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L159C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 160,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L160C8"
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
                      "cssClass": "identifier",
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
                  "number": 161,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 162,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 163,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns True is the object does not hold a null reference\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 160,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Not_Null",
          "qualifier": "",
          "line": 212,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 212,
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
                      "text": "Not_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L212C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 213,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L213C8"
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
                      "cssClass": "identifier",
                      "text": "Weak_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L391C9"
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
                  "number": 214,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 215,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 216,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns True if the object does not hold a null reference.  Take care\n"
                },
                {
                  "kind": "span",
                  "text": "if the result is True.  If a result of True is mandatory, consider\n"
                },
                {
                  "kind": "span",
                  "text": "creating a Shared_Access object from the Weak_Access object and check\n"
                },
                {
                  "kind": "span",
                  "text": "Not_Null with it.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 213,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Not_Null",
          "qualifier": "",
          "line": 263,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 263,
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
                      "text": "Not_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L263C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 264,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L264C8"
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
                      "cssClass": "identifier",
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L402C9"
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
                  "number": 265,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Boolean"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 266,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 267,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns True if the object does not hold a null reference\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 264,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Reference",
          "qualifier": "",
          "line": 120,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 120,
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
                      "text": "Reference",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L120C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 121,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L121C8"
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
                      "cssClass": "identifier",
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
                  "number": 122,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Item_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L45C9"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 123,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 124,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns a dereferenced view of the Share_Access object's reference\n"
                },
                {
                  "kind": "span",
                  "text": "NOTE:  In the future (when GNAT is fixed) this will return a\n"
                }
              ]
            },
            {
              "kind": "code",
              "children": [
                {
                  "number": 1,
                  "children": [
                    {
                      "kind": "span",
                      "text": "variable of type References.Reference:"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "   package References is new Access_Types.References(Item_Type);"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "   function Reference"
                    }
                  ]
                },
                {
                  "number": 4,
                  "children": [
                    {
                      "kind": "span",
                      "text": "      (Self : in Shared_Access)"
                    }
                  ]
                },
                {
                  "number": 5,
                  "children": [
                    {
                      "kind": "span",
                      "text": "       return References.Reference;"
                    }
                  ]
                },
                {
                  "number": 6,
                  "children": [
                    {
                      "kind": "span",
                      "text": ""
                    }
                  ]
                },
                {
                  "number": 7,
                  "children": [
                    {
                      "kind": "span",
                      "text": "Then all calls of Object.Reference.all will need to be replaced"
                    }
                  ]
                },
                {
                  "number": 8,
                  "children": [
                    {
                      "kind": "span",
                      "text": "with just Object.Reference"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 121,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Reference",
          "qualifier": "",
          "line": 241,
          "column": 13,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 241,
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
                      "text": "Reference",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L241C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 242,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L242C8"
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
                      "cssClass": "identifier",
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L402C9"
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
                  "number": 243,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "       "
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
                      "text": "Item_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L45C9"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 244,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 245,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Returns a dereferenced view of the Unique_Access object's reference\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 242,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Set_Null",
          "qualifier": "",
          "line": 105,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
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
                      "text": "Set_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L105C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 106,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L106C8"
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
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
                  "number": 107,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 108,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Releases the Shared_Access object's control of the reference.  The\n"
                },
                {
                  "kind": "span",
                  "text": "Shared_Access object is set to null.  If it was the last Shared_Access\n"
                },
                {
                  "kind": "span",
                  "text": "object in control of the reference, the reference is finalized.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 106,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Set_Null",
          "qualifier": "",
          "line": 185,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 185,
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
                      "text": "Set_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L185C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 186,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L186C8"
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
                      "text": "Weak_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L391C9"
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
                  "number": 187,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 188,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Releases the Weak_Access object's control of the reference count.  The\n"
                },
                {
                  "kind": "span",
                  "text": "Weak_Access object is set to null.  If it was the last Weak_Access\n"
                },
                {
                  "kind": "span",
                  "text": "object in control of the reference count, the reference count is\n"
                },
                {
                  "kind": "span",
                  "text": "finalized.  The presence of any number of Shared_Access objects counts\n"
                },
                {
                  "kind": "span",
                  "text": "as one Weak_Access in this context.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 186,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Set_Null",
          "qualifier": "",
          "line": 235,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 235,
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
                      "text": "Set_Null",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L235C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 236,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Self",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L236C8"
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
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L402C9"
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
                  "number": 237,
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
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 238,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         Inline => True"
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
                  "text": "Releases the Unique_Access object's control of the reference.  The\n"
                },
                {
                  "kind": "span",
                  "text": "reference is finalized.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Self",
              "line": 236,
              "column": 8,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Swap",
          "qualifier": "",
          "line": 169,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 169,
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
                      "text": "Swap",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L169C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L169C20"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L169C26"
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
                      "text": "Shared_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L380C9"
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
                  "text": "Swaps two Shared_Access objects.  This is generally faster\n"
                },
                {
                  "kind": "span",
                  "text": "than simple assignement as it doesn't require any calls to\n"
                },
                {
                  "kind": "span",
                  "text": "Finalize or Adjust, and it doesn't require any protected\n"
                },
                {
                  "kind": "span",
                  "text": "operations.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 169,
              "column": 20,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 169,
              "column": 26,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Shared_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L81C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Swap",
          "qualifier": "",
          "line": 222,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 222,
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
                      "text": "Swap",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L222C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L222C19"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L222C25"
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
                      "text": "Weak_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L391C9"
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
                  "text": "Swaps two Weak_Access objects.  This is generally faster\n"
                },
                {
                  "kind": "span",
                  "text": "than simple assignement as it doesn't require any calls to\n"
                },
                {
                  "kind": "span",
                  "text": "Finalize or Adjust, and it doesn't require any protected\n"
                },
                {
                  "kind": "span",
                  "text": "operations.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 222,
              "column": 19,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 222,
              "column": 25,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Weak_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L88C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Swap",
          "qualifier": "",
          "line": 270,
          "column": 14,
          "src": "srcs/bullfrog-access_types-smart_access.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 270,
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
                      "text": "Swap",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L270C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L270C19"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L270C25"
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
                      "text": "Unique_Access",
                      "href": "docs/bullfrog__access_types__smart_access___spec.html#L402C9"
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
                  "text": "Swaps two Unique_Access objects\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 270,
              "column": 19,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 270,
              "column": 25,
              "type": {
                "label": "Bullfrog.Access_Types.Smart_Access.Unique_Access",
                "docHref": "docs/bullfrog__access_types__smart_access___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Dispatching subprograms"
    },
    {
      "entities": [
        {
          "label": "Make",
          "href": "../docs/bullfrog__access_types__smart_access___make___spec.html#L279C12",
          "qualifier": "",
          "summary": [
          ],
          "description": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This package provides all the constructing operations for Smart_Acess\n"
                },
                {
                  "kind": "span",
                  "text": "types.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Utilities",
          "href": "../docs/bullfrog__access_types__smart_access___utilities___spec.html#L333C12",
          "qualifier": "",
          "summary": [
          ],
          "description": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This package provides special purpose non-primitive operations for\n"
                },
                {
                  "kind": "span",
                  "text": "Smart_Access types.\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Nested packages"
    }
  ]
};