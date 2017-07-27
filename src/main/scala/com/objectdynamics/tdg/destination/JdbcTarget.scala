/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 4/20/11
 * Time: 12:46 AM
 */
package com.objectdynamics.tdg.destination

;

case class JdbcTarget(jdbcUrl: String,
                      user: String,
                      pswd: String,
                      schema: String,
                      catalog: String) extends IDataTarget {
  def this(jdbcUrl: String, user: String, pswd: String) = {
    this(jdbcUrl, user, pswd, null, null);
  }
}
