package com.protegra_ati.agentservices.core.util.serializer;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.serializers.CompatibleFieldSerializer;
import com.esotericsoftware.kryo.serializers.MapSerializer;
import com.protegra_ati.agentservices.core.schema.Data;
import com.protegra_ati.agentservices.core.schema.Post;
import com.protegra_ati.agentservices.core.util.serializer.helper.*;
import org.apache.commons.pool.BasePoolableObjectFactory;

import java.util.HashMap;

/**
 * Kryo factory used for a serializer pool, creates new kryo serializer
 */
public class SerializerPoolableFactory extends BasePoolableObjectFactory
{
    // TODO all serializer have to be initialized from a configuration
    private Kryo newKryo()
    {
        Kryo _kryo = new Kryo();//ReflectionFactorySupport();
        _kryo.setReferences( false );  // dont use references of the same object
        _kryo.setRegistrationRequired( false );
        _kryo.setDefaultSerializer( CompatibleFieldSerializer.class );//CompatibleFieldSerializerReflectionFactorySupport.class );
        _kryo.register( OptionSerializer.FITS_TO(), new OptionSerializer(), 1000 );

        _kryo.register( SomeOfStringSerializer.FITS_TO(), new SomeOfStringSerializer(), 1001 );
        SomeSerializer someSer = new SomeSerializer();
        //someSer.setGenerics( _kryo, new Class[]{ String.class } );
        _kryo.register( SomeSerializer.FITS_TO(), someSer, 1001 );
//

        _kryo.register( EventKeySerializer.FITS_TO(), new EventKeySerializer(), 1002 );
        _kryo.register( JodaDateTimeSerializer.FITS_TO(), new JodaDateTimeSerializer(), 1003 );
        MapSerializer ms = new MapSerializer();
        //ms.setGenerics( _kryo, new Class[]{ String.class, Data.class } );
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
        //jConversionSer.setGenerics( _kryo, new Class[]{ String.class, Post.class } );
        jConversionSer.setElementsCanBeNull( true );
        _kryo.register( JavaConversionsSeqWrapperSerializer.FITS_TO(), jConversionSer, 1014 );
        _kryo.register( NilSerializer.FITS_TO(), new NilSerializer(), 10015 );
        registerAdditionalClassesImpl(_kryo);
        _kryo.setAutoReset( true );


        // TODO load from config
        return _kryo;
    }
    protected void registerAdditionalClassesImpl(Kryo kryo)
    {
        //base impl doesn't have any additional classes

    }

    @Override
    public Object makeObject()
    {
        return newKryo();
    }


}
