package com.protegra_ati.agentservices.core.util.serializer;

import com.protegra_ati.agentservices.core.util.serializer.helper.UUIDSerializer;
import com.protegra_ati.agentservices.core.util.ReportingImpl4Java;
import com.protegra_ati.agentservices.core.util.serializer.helper.*;
import com.protegra.agentservicesstore.util.Severity;
import com.protegra_ati.agentservices.core.schema.*;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.serializers.CompatibleFieldSerializer;
import com.esotericsoftware.kryo.serializers.MapSerializer;

import org.apache.commons.pool.BasePoolableObjectFactory;
import org.apache.commons.pool.impl.StackObjectPool;
import org.apache.ws.commons.util.Base64;

import java.io.*;
import java.util.HashMap;
import java.util.UUID;

/**
 * Serialization factory which uses Kryo API, with a serializer pool in backend (comparable with database connection pool ), allows fast thread safe way to use Kryo API.
 * Necessary because Kryo API isn't thread safe
 */
public final class KryoSerializer extends AbstractToStringSerializer
{
 // TODO eventually logging has to be restored
    private final StackObjectPool pool;
    private final ReportingImpl4Java logger;
    private static final String HEADER_4_STRING_SERIALIZATION = "KryoSerializer_";

    private static class Holder
    {
        private static final KryoSerializer INSTANCE = new KryoSerializer();
    }

    private KryoSerializer()
    {
        pool = new StackObjectPool( new SerializerPoolableFactory(), 500, 200 );
        logger = new ReportingImpl4Java();
    }

    /**
     * Creates on demand instance of serialization factory,
     *
     * @return always fully initialized singleton instance
     */
    public static KryoSerializer getInstance()
    {
        return Holder.INSTANCE;
    }

    /**
     * Serializes object into stream using 'Kryo' API
     *
     * @param toBeSerialized object
     * @param intoStream     target
     */
    public void serialize( Object toBeSerialized, OutputStream intoStream )
    {
        Kryo serializer = null;
        final Output buffer = new Output( intoStream, 5000 );
        try {
            try {
                serializer = (Kryo) pool.borrowObject();

            } catch ( Exception e ) {
                //   logger.report( "can't borrow serializer from a pool" + e.getMessage(), Severity.Error() );
            }
            serializer.writeClassAndObject( buffer, toBeSerialized );


        } catch ( Exception e ) {
            e.printStackTrace();
            // logger.report( "object " + toBeSerialized + "  can't be serialized:" + e.getMessage(), Severity.Error() );
        } catch ( Error er ) {
            er.printStackTrace();
            //  logger.report( "object " + toBeSerialized + "  can't be serialized:" + er.getMessage(), Severity.Error() );
        } finally {

            if ( buffer != null ) {
                try {
                    buffer.flush();
                    buffer.clear();
                    buffer.close();
                } catch ( Exception e ) {
                    //   logger.report( "buffer can't be closed:" + e.getMessage(), Severity.Warning() );
                }
            }

            if ( serializer != null ) {
                try {
                    pool.returnObject( serializer );
                } catch ( Exception e ) {
                    //  logger.report( "can't return serializer back to the pool:" + e.getMessage(), Severity.Warning() );
                }
            }
        }
    }

    /**
     * Serializes object into String using 'Kryo' API and after it base 64 encoder
     *
     * @param objToBeSerialized to be serialized
     * @return base64 encoded object byte stream
     */
    protected String serializeRaw( Object objToBeSerialized )
    {
        String uid5CharLong = UUID.randomUUID().toString().substring( 0, 5 );
        //System.err.println( "#####-serialize KRYO--ID:" + uid5CharLong + "--: " + objToBeSerialized );
        // logger.report( "#####-serialize KRYO--ID:" + uid5CharLong + "--: " + objToBeSerialized, Severity.Warning() );
        BufferedOutputStream oos = null;
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        //try {
        oos = new BufferedOutputStream( baos );
        // } catch ( IOException e ) {/*can never happen here*/}
        serialize( objToBeSerialized, oos );  // closes the stream
        final byte[] bArray = baos.toByteArray();
        serializationErrorCheck( bArray, objToBeSerialized );
        final String encodedMsg = new String( Base64.encode( bArray ) );
        sizeWarning( encodedMsg, ( objToBeSerialized ) );
        return uid5CharLong + encodedMsg;
    }

    public String getHeader()
    {
        // returns always the type
        return HEADER_4_STRING_SERIALIZATION;
    }

    /**
     * deserializes object of type T from given InputStream
     *
     * @param fromInputStream source
     * @param <T>             desired type
     * @return object of type T or null
     */
    public <T> T deserialize( InputStream fromInputStream )
    {
        Kryo serializer = null;
        Object data = null;
        try {
            try {
                serializer = (Kryo) pool.borrowObject();
            } catch ( Exception e ) {
                //     logger.report( "ERROR: can't borrow serializer from a pool" + e.getMessage(), Severity.Error() );
            }
            final Input buffer = new Input( fromInputStream, 5000 );
            data = serializer.readClassAndObject( buffer );
            buffer.rewind();
            buffer.close();
            return (T) data;
        } catch ( Exception e ) {
            e.printStackTrace();
            //    logger.report( "ERROR: object " + data + " can't be deserialized:" + e.getMessage(), Severity.Error() );
        } catch ( Error er ) {
            er.printStackTrace();
            //   logger.report( "ERROR: object " + data + " can't be deserialized:" + er.getMessage(), Severity.Error() );
        } finally {
            if ( fromInputStream != null ) {
                try {
                    fromInputStream.close();
                } catch ( IOException e ) {
                    //  logger.report( "buffer can't be closed:" + e.getMessage(), Severity.Warning() );
                }
            }

            if ( serializer != null ) {
                try {
                    pool.returnObject( serializer );
                } catch ( Exception e ) {
                    //   logger.report( "can't return serializer back to the pool:" + e.getMessage(), Severity.Warning() );
                }
            }
        }
        return (T) null;
    } // not reachable


    protected <T> T deserializeRaw( String source )
    {
        try {
            String uid = source.substring( 0, 5 );
            //System.err.println( "#####-deserialize KRYO-- UID:" + uid );
            //   logger.report( "#####-deserialize KRYO-- UID:" + uid, Severity.Warning() );

            final byte[] byteArrayMsg = Base64.decode( source.substring( 5, source.length() ) );
            final InputStream ois = new BufferedInputStream( new ByteArrayInputStream( byteArrayMsg ) );
            final T obj = deserialize( ois ); // closes the stream
            //System.err.println( "#####-deserialize KRYO--ID:" + uid + " ----: " + obj );
            // logger.report( "#####-deserialize KRYO--ID:" + uid + " ----: " + obj, Severity.Warning() );
            return obj;
        } catch ( Base64.DecodingException e ) {
            //   logger.report( "ERROR: object can't be deserialized due to base64 decoding problem:" + e.getMessage(), Severity.Error() );
        } catch ( Exception e ) {
            //   logger.report( "object can't be deserialized:" + e.getMessage(), Severity.Error() );
        }
        return (T) null;
    }

    private final void sizeWarning( String encodedMsg, Object obj )
    {
        long bytesInAKilobyte = 1024;
        long maxBytes = 100 * bytesInAKilobyte;
        //each char roughly 1 byte
        if ( encodedMsg.length() > maxBytes ) {
            //   logger.report( "serialized message is more than " + maxBytes / bytesInAKilobyte + " KB for obj " + obj.toString(), Severity.Warning() );
        }
    }

}


/**
 * Kryo factory used for a serializer pool, creates new kryo serializer
 */
class SerializerPoolableFactory extends BasePoolableObjectFactory
{
    // TODO all serializer have to be initialized from a configuration
    private static final Kryo newKryo()
    {
        Kryo _kryo = new Kryo();//ReflectionFactorySupport();
        _kryo.setReferences( false );  // dont use references of the same object
        _kryo.setRegistrationRequired( false );
        _kryo.setDefaultSerializer( CompatibleFieldSerializer.class );//CompatibleFieldSerializerReflectionFactorySupport.class );
        _kryo.register( OptionSerializer.FITS_TO(), new OptionSerializer(), 1000 );
        //  _kryo.register( SomeSerializer.FITS_TO(), new SomeSerializer(), 1001 );

        _kryo.register( SomeOfStringSerializer.FITS_TO(), new SomeOfStringSerializer(), 1001 );
        SomeSerializer someSer = new SomeSerializer();
        someSer.setGenerics( _kryo, new Class[]{ String.class } );
        _kryo.register( SomeSerializer.FITS_TO(), someSer, 1001 );
//

        _kryo.register( EventKeySerializer.FITS_TO(), new EventKeySerializer(), 1002 );
        _kryo.register( JodaDateTimeSerializer.FITS_TO(), new JodaDateTimeSerializer(), 1003 );
        MapSerializer ms = new MapSerializer();
        ms.setGenerics( _kryo, new Class[]{ String.class, Data.class } );
        _kryo.register( HashMap.class, ms, 1004 );
        _kryo.register( EnumerationSerializer.FITS_TO(), new EnumerationSerializer(), 1005 );
        _kryo.register( EnumerationSerializer.FITS_TO1(), new EnumerationSerializer(), 1006 );
        _kryo.register( NoneSerializer.FITS_TO(), new NoneSerializer(), 1007 );
        _kryo.register( IdentificationSerializer.FITS_TO(), new IdentificationSerializer(), 1008 );
        _kryo.register( UUIDSerializer.FITS_TO(), new UUIDSerializer(), 1009 );
        //  _kryo.register( MockMessageSerializer.FITS_TO(), new MockMessageSerializer(), 1010 );
        // _kryo.register( MockEventKeySerializer.FITS_TO(), new MockEventKeySerializer(), 1011 );
        //_kryo.register( MockConnSerializer.FITS_TO(), new  MockConnSerializer(), 1012 );
        // _kryo.register( MockAgentCnxnProxySerializer.FITS_TO(), new MockAgentCnxnProxySerializer(), 1013 );
        _kryo.register( AgentCnxnProxySerializer.FITS_TO(), new AgentCnxnProxySerializer(), 1013 );
        JavaConversionsSeqWrapperSerializer jConversionSer = new JavaConversionsSeqWrapperSerializer();
//        jConversionSer.setGenerics( _kryo, new Class[]{ String.class, Post.class, WatchListItem.class } );
        jConversionSer.setGenerics( _kryo, new Class[]{ String.class, Post.class } );
        jConversionSer.setElementsCanBeNull( true );
        _kryo.register( JavaConversionsSeqWrapperSerializer.FITS_TO(), jConversionSer, 1014 );
        _kryo.register( NilSerializer.FITS_TO(), new NilSerializer(), 10015 );
        _kryo.setAutoReset( true );

        // TODO load from config
        return _kryo;
    }

    @Override
    public Object makeObject()
    {
        return newKryo();
    }


}


