

Algorithms:

GenerateData ( TreeRequest request, schema: ITestDataSchema ){

  TestData testData = new TestData( defaultStorage );
  for ( i=0 to request.rows ){
      testData = GenerateRowForRequest( testData, i, request )
  }

}

GenerateRowForRequest( testData: TestData, i: Int, request: TreeRequest, schema : ITestDataSchema) {
    /// Create output Row
    newRow = new DataRow();
    val dataSetName = request.name()
    val dataSetSpec:IDataSetSpec = schema.dataSetSpec(dataSetName);
    var i:Int = 0;
    dataSetSpec.fields.forEach((field: IDataField) => {
      if ( field.isNormal() ){
        // get generator for field
        // generate value
      }
    })




}
