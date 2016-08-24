/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 4/21/11
 * Time: 1:25 AM
 */
package com.objectdynamics.tdg.util

import com.objectdynamics.tdg.destination.JdbcTarget
import org.apache.ddlutils._
import javax.sql._
import org.apache.ddlutils.Platform
import org.apache.ddlutils.model._
import com.objectdynamics.tdg.builder.model._
import org.apache.commons.dbcp._
;

trait JdbcUtils
{

    def databaseFromTarget(targetDb: JdbcTarget): Database =
    {
        val ds = createDataSource(targetDb.jdbcUrl, targetDb.user, targetDb.pswd, targetDb.catalog);
        readDatabase(ds);
    }

    def createDataSource(jdbcUrl: String,
                         user: String,
                         pswd: String,
                         catalog: String): DataSource =
    {
        val bds = new BasicDataSource();
        bds.setUrl(jdbcUrl);
        bds.setPassword(pswd);
        bds.setUsername(user);
        bds.setDefaultCatalog(catalog);
        bds;
    }

    def readDatabase(dataSource: DataSource): Database =
    {
        val platform: Platform = PlatformFactory.createNewPlatformInstance(dataSource);
        return platform.readModelFromDatabase("model");
    }

    def createTable(db: Database, target: JdbcTarget, tableName: String, dss: IDataSetSpec): Table =
    {

        var t: Table = new Table();
        t.setCatalog(target.catalog);
        t.setName(tableName);
        t.setSchema(target.schema);
        t.setType("table");

        t = addColumnsToTable(t, dss.fields);
        db.addTable(t);
        t;

    }

    def addColumnsToTable(t: Table, fields: List[IDataField]): Table =
    {

        //TODO DO THIS !
        ///DDOOOO  THIS !!!!   MAAAAAAANNNNN  !!!
        t
    }
}