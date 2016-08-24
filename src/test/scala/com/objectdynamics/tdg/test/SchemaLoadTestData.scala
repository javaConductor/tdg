/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/1/11
 * Time: 2:04 AM
 */
package com.objectdynamics.tdg.test

;

trait SchemaLoadTestData
{

    def testSchema = """ [
    {
        "dataSetType": "rdbms",
        "name": "Employee",
        "options": {},
        "relationships":[
            {
                "dataSetName" : "Department",
                "fieldMap": {
                    "deptId":"id"
                },
                "disbursementSpec": {
                    "percentages":[1,2,3,25,25,25,19]
                },
                "active":true
            }
        ],
        "fields": [
            {
                "name": "id",
                "type":"Integer",
                "PK":"true",
                "generator":"autoIncrement"
            },
            {
                "name":"deptId",
                "type":"Integer"
            },
            {
                "name":"salary",
                "type":"Integer",
                "min" : 400,
                "max" : 404,
                "generator":"randomInteger",
                "unique":false

            },
            {
                "name":"name",
                "type":"Text",
                "unique":true,
                "maxLength":30,
                "generator":"rotatedStrings",
                "disbursementSpec": {
                    "percentages":[25,25,25,25]
                },
                "data":["Math","Reading","History","Sports"]
            }
        ]
    },
    {
        "dataSetType": "rdbms",
        "name": "Department",
        "options": {},
        "relationships":[
            {
                "dataSetName" : "Employee",
                "fieldMap": {
                    "mgrId":"id"
                },
                "disbursementSpec": {
                    "percentages":[1,2,3,25,25,25,19]
                },
                "active":true
            }
        ],
        "fields": [
            {
                "name": "id",
                "type":"Integer",
                "PK":"true",
                "generator":"autoIncrement"
            },
            {
                "name":"mgrId",
                "type":"Integer",
                "generator":"randomInteger",
                "unique":true
            }
        ]
    },
    {
        "dataSetType": "rdbms",
        "name": "RetirementAccounts",
        "options": {},
        "relationships":[
            {
                "fieldMap": {
                    "employeeId":"id"
                },

                "active":true,
                "dataSetName":"Employee",
                "disbursementSpec": {
                    "ratio": [1, 1]
                }

            }
        ],
        "fields": [
            {
                "name": "id",
                "type":"Integer",
                "PK":"true",
                "generator":"autoIncrement"
            },
            {
                "name":"employeeId",
                "type":"Integer",
                "unique":true
            }
        ]
    }

]""";


    def schemaWithLike = """ [
    {
        "dataSetType": "rdbms",
        "name": "Employee",
        "options": {},
        "fields": [
            {
                "name": "id",
                "type":"Integer",
                "PK":"true",
                "generator":"autoIncrement"
            },
            {
                "name":"deptId",
                "type":"Integer"
            },
            {
                "name":"tool1",
                "type":"Text",
                "unique":true,
                "maxLength":30,
                "generator":"rotatedStrings",
                "disbursementSpec": {
                    "percentages":[25,25,25,25]
                },
                "data":["hammer","saw","wrench","drill"]
            },

            {
                "name":"tool2",
                "like":"tool1"
            },
            {
                "name":"salary",
                "type":"Integer",
                "min" : 400,
                "max" : 404,
                "generator":"randomInteger",
                "unique":false

            },
            {
                "name":"previousSalary",
                "like": "salary"
            }
        ]
    }

]""";
}