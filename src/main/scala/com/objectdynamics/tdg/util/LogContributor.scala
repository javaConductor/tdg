package com.objectdynamics.tdg.util

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 12/31/10
 * Time: 2:22 AM
 * To change this template use File | Settings | File Templates.
 */

trait LogContributor
{

    def log(s: String)
    {
        System.out.println(s);
    }

    def error(s: String, t: Throwable)
    {
        error(s);
        t.printStackTrace(System.err);

    }

    def error(s: String)
    {
        System.err.println(s);
    }
}

trait NoLogContributor extends LogContributor
{

    override def log(s: String)
    {
        //System.out.println(s);
    }

}