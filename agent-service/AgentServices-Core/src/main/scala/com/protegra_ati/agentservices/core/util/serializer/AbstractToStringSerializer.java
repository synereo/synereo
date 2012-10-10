package com.protegra_ati.agentservices.core.util.serializer;

public abstract class AbstractToStringSerializer
{

    /**
     * returns a static header for serialized objects using given serialization strategy
     *
     * @return add on to find out which deserializer has to be used
     */
    public abstract String getHeader();

    /**
     * Adds header to the according to the internal serialization strategy created string
     *
     * @param objToBeSerialized to be serialized
     * @return as a string with a header
     */
    public String serialize( Object objToBeSerialized )
    {
        return this.getHeader() + this.serializeRaw( objToBeSerialized );
    }

    /**
     * Serializes to a string without header according to a serialization strategy.
     * someone who desirializes such string has to know what object type is in it and also which serialization strategy was used
     *
     * @param objToBeSerialized to be serialized
     * @return as a string without a header
     */
    protected abstract String serializeRaw( Object objToBeSerialized );

    /**
     * Deserializes object of type T from a string with a header according to a serialization strategy.
     * Header will be separated before serialization
     *
     * @param sourceWithHeader HEADER + base64 encoded string, with a header
     * @param <T>              type of the deserialized object
     * @return object of type T or null in case of error
     */
    public <T> T deserialize( String sourceWithHeader )
    {
        return this.deserializeRaw( sourceWithHeader.substring( this.getHeader().length() ,sourceWithHeader.length() ));
    }

    /**
     * Deserializes object of type T from a string without header according to a serialization strategy.
     *
     * @param source base64 encoded string, without a header
     * @param <T>    type of the deserialized object
     * @return object of type T or null in case of error
     */
    protected abstract <T> T deserializeRaw( String source );


    protected void serializationErrorCheck( byte[] bArray, Object obj )
    {
        if ( obj == null )
            throw new NullPointerException( "ERROR: Null object can't be serialized" );
        if ( bArray == null || bArray.length == 0 )
            throw new IllegalStateException( "ERROR: Serialization to byte array  of the object " + obj.toString() + " failed" );
    }

}
