package com.protegra_ati.agentservices.core.util.cloner;

import com.rits.cloning.Cloner;

/**
 * Cloner factory, with new registered immutable classes.
 * Necessary because Kryo API isn't thread safe
 */
public final class ClonerFactory
{
    private final Cloner cloner;

    private static class Holder
    {
        private static final ClonerFactory INSTANCE = new ClonerFactory();
    }

    private ClonerFactory()
    {
        cloner = new Cloner();
        configureClonerFactory( cloner );
    }


    public <T> T createDepClone( final T o )
    {
        return cloner.deepClone( o );
    }

    public <T> T createShallowClone( final T o )
    {
        return cloner.shallowClone( o );

    }

    public <T> T createNewInstance( final Class<T> c )
    {
        return cloner.newInstance( c );
    }


    private void configureClonerFactory( final Cloner cloner )
    {   // DODO use ConfigurationManager to get class name of the ClonerFactoryConfigurator implementation
        ClonerFactoryConfigurator configurator = null; // TODO load class from config file
        if ( configurator == null )
            configurator = new DefaultClonerFactoryConfigurator();
        configurator.configure( cloner );
    }

    /**
     * Creates on demand instance of cloner factory,
     *
     * @return always fully initialized singleton instance
     */
    public static ClonerFactory getInstance()
    {
        return Holder.INSTANCE;
    }

}


