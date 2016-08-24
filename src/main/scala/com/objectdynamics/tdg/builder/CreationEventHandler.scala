package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.generators._

/**
 * Created by IntelliJ IDEA.
 * User: lcollins
 * Date: 8/18/11
 * Time: 1:10 AM
 * To change this template use File | Settings | File Templates.
 */

trait CreationEventHandler
{


    var fieldListeners = Map[String, List[(GeneratedValue) => Unit]]();

    def addFieldCreationListener(fieldName: String, f: (GeneratedValue) => Unit) =
    {

        val soFar = fieldListeners.get(fieldName) match
        {
            case Some(flist: List[(GeneratedValue) => Unit]) => flist
            case _ => Nil;
        }

        val newList = f :: soFar;
        fieldListeners += (fieldName -> newList);
    }

    def notifyFieldCreationListeners(value: NamedGeneratedValue) =
    {
        val soFar = fieldListeners.get(value.name) match
        {
            case Some(flist: List[(GeneratedValue) => Unit]) => flist
            case _ => Nil;
        }
        soFar foreach ((f) => f(value))
    }

}