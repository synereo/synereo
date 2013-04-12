package com.protegra_ati.agentservices.core.util.serializer;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.serializers.CompatibleFieldSerializer;
import com.esotericsoftware.kryo.serializers.MapSerializer;
import com.protegra_ati.agentservices.core.schema.Data;
import com.protegra_ati.agentservices.core.schema.Post;
import com.protegra_ati.agentservices.core.util.serializer.helper.*;
import com.protegra_ati.agentservices.core.util.serializer.helper.NoneSerializer;
import org.apache.commons.pool.BasePoolableObjectFactory;
import scala.Enumeration;

import java.util.HashMap;

/**
 * Kryo factory used for a serializer pool, creates new kryo serializer
 */
public class SerializerPoolableFactory extends BasePoolableObjectFactory
{
    // TODO all serializer have to be initialized from a configuration
    private Kryo newKryo()
    {
        Kryo _kryo = new Kryo();
        _kryo.setReferences( false );
        _kryo.setRegistrationRequired( false );
        _kryo.setDefaultSerializer( CompatibleFieldSerializer.class );

        _kryo.register(NoneSerializer.FITS_TO(), new NoneSerializer());
        _kryo.register(SomeSerializer.FITS_TO(), new SomeSerializer());
        _kryo.register( EventKeySerializer.FITS_TO(), new EventKeySerializer());
        _kryo.register( JodaDateTimeSerializer.FITS_TO(), new JodaDateTimeSerializer());
        _kryo.register( HashMap.class, new MapSerializer());
        _kryo.addDefaultSerializer(EnumerationSerializer.FITS_TO(), EnumerationSerializer.ENUM_SER_CLASS_OF());
        //_kryo.register( EnumerationSerializer.FITS_TO(), new EnumerationSerializer());
        //_kryo.register( EnumerationSerializer.FITS_TO1(), new EnumerationSerializer());
        _kryo.register( IdentificationSerializer.FITS_TO(), new IdentificationSerializer());
        _kryo.register( UUIDSerializer.FITS_TO(), new UUIDSerializer());
        _kryo.register( AgentCnxnProxySerializer.FITS_TO(), new AgentCnxnProxySerializer());
        JavaConversionsSeqWrapperSerializer jConversionSer = new JavaConversionsSeqWrapperSerializer();
        jConversionSer.setElementsCanBeNull( true );
        _kryo.register( JavaConversionsSeqWrapperSerializer.FITS_TO(), jConversionSer);
        _kryo.register( NilSerializer.FITS_TO(), new NilSerializer());
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
