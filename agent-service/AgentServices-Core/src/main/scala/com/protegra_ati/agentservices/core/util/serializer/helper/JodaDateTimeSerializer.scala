package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.chrono._
import com.protegra.agentservicesstore.extensions.StringExtensions._


class JodaDateTimeSerializer() extends Serializer[ DateTime ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }

  override def write(kryo: Kryo, output: Output, obj: DateTime): Unit =
  {
    System.err.println("KRYO JodaDateTime in USE! WRITE")
    if ( obj == null )
      kryo.writeObject(output, JodaDateTimeSerializer.NULL)
    else {
      kryo.writeObject(output, JodaDateTimeSerializer.NOT_NULL)
      kryo.writeObject(output, obj.getMillis)
      writeChronology(kryo, output, obj.getChronology)
      writeTimeZone(kryo, output, obj.getZone)
    }

  }

  override def read(kryo: Kryo, input: Input, typ: Class[ DateTime ]): DateTime =
  {
    System.err.println("KRYO JodaDateTime in USE! READ")

    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( label == JodaDateTimeSerializer.NULL ) return null
    val millis: Long = kryo.readObject(input, classOf[ Long ])
    val chronology = readChronology(kryo, input)
    val tz = readTimeZone(kryo, input)
    return new DateTime(millis, chronology.withZone(tz))

  }

  // ---- chronology
  private def readChronology(kryo: Kryo, buffer: Input): Chronology =
  {
    val chronologyKey = kryo.readObject(buffer, classOf[ String ])
    JodaDateTimeSerializer.chronologiesMap.get(chronologyKey) match {
      case None => return JodaDateTimeSerializer.defaultChronology
      case Some(chronology) => {
        return chronology
      }
    }
  }

  private def writeChronology(kryo: Kryo, buffer: Output, chronology: Chronology): Unit =
  {
    if ( chronology == null )
      kryo.writeObject(buffer, "")
    else
      kryo.writeObject(buffer, chronology.getClass.getName.trimPackage.toCamelCase)
  }

  //---- time zone
  private def readTimeZone(kryo: Kryo, buffer: Input): DateTimeZone =
  {
    val tz = kryo.readObject(buffer, classOf[ String ])
    if ( "".equals(tz) ) return DateTimeZone.getDefault()
    else return DateTimeZone.forID(tz.toString())
  }

  private def writeTimeZone(kryo: Kryo, buffer: Output, dateTimeZone: DateTimeZone): Unit =
  {
    if ( dateTimeZone == null || dateTimeZone == DateTimeZone.getDefault() )
      kryo.writeObject(buffer, "")
    else
      kryo.writeObject(buffer, dateTimeZone.getID())
  }

}

object JodaDateTimeSerializer
{
  val MILLIS = "millis";
  val DATE_TIME = "dt";
  val CHRONOLOGY = "ch";
  val TIME_ZONE = "tz";
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ DateTime ] = classOf[ DateTime ]

  private val defaultChronology = ISOChronology.getInstance()
  private val chronologiesMap: Map[ String, _ <: Chronology ] = Map(
    classOf[ BuddhistChronology ].getName.trimPackage.toCamelCase -> BuddhistChronology.getInstance(),
    classOf[ CopticChronology ].getName.trimPackage.toCamelCase -> CopticChronology.getInstance(),
    classOf[ GJChronology ].getName.trimPackage.toCamelCase -> GJChronology.getInstance(),
    classOf[ GregorianChronology ].getName.trimPackage.toCamelCase -> GregorianChronology.getInstance(),
    classOf[ ISOChronology ].getName.trimPackage.toCamelCase -> defaultChronology,
    classOf[ IslamicChronology ].getName.trimPackage.toCamelCase -> IslamicChronology.getInstance(),
    classOf[ JulianChronology ].getName.trimPackage.toCamelCase -> JulianChronology.getInstance())
}
