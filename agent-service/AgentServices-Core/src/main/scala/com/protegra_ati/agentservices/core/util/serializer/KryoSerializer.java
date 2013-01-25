package com.protegra_ati.agentservices.core.util.serializer;

import com.protegra_ati.agentservices.core.util.serializer.helper.UUIDSerializer;
import com.protegra_ati.agentservices.core.util.ReportingImpl4Java;
import com.protegra_ati.agentservices.core.util.serializer.helper.*;
import com.protegra_ati.agentservices.store.util.Severity;
import com.protegra_ati.agentservices.core.schema.*;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.serializers.CompatibleFieldSerializer;
import com.esotericsoftware.kryo.serializers.MapSerializer;

import org.apache.commons.pool.BasePoolableObjectFactory;
import org.apache.commons.pool.impl.StackObjectPool;


import java.io.*;
import java.util.HashMap;
import java.util.UUID;

/**
 * Serialization factory which uses Kryo API, with a serializer pool in backend (comparable with database connection pool ), allows fast thread safe way to use Kryo API.
 * Necessary because Kryo API isn't thread safe
 */
public final class KryoSerializer extends KryoSerializerBase
{
 // TODO eventually logging has to be restored
    private final StackObjectPool poolImpl;
    protected final ReportingImpl4Java logger;

    private static class Holder
    {
        private static final KryoSerializerBase INSTANCE = new KryoSerializer();
    }

    private KryoSerializer()
    {
        poolImpl = new StackObjectPool( new SerializerPoolableFactory(), 500, 200 );
        logger = new ReportingImpl4Java();
    }
    protected StackObjectPool getPoolImpl()
    {
        return poolImpl;
    }

    /**
     * Creates on demand instance of serialization factory,
     *
     * @return always fully initialized singleton instance
     */
    public static KryoSerializerBase getInstance()
    {
        return Holder.INSTANCE;
    }


}





