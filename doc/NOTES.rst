======
tlserl
======

``tlserl`` is a basic CORBA Telecom Log Service implementation
written in Erlang.

RUNNING
=======

High level
----------

1. Start orber::

   orber:jump_start(1234).

2. Install dsLogAdminApp into the IFR using::

   dsLogAdminApp:install().

3. Start the dsLogAdminApp application::

   dsLogAdminApp:start().

4. Create a new log factory::

   F = dsLogAdminApp:start_logmgr().

5. Create a new log using this factory::

    {L,Id}='DsLogAdmin_BasicLogFactory':create(F, 0, 0).

6. To retrieve the IOR associated to the log manager instance, use::

    corba:object_to_string(F).


Low level
---------

1. Start orber::

    orber:jump_start(1234).

2. Create a new log factory::

    F='DsLogAdmin_BasicLogFactory':oe_create().

3. Create a new log using this factory::

    {L,Id}='DsLogAdmin_BasicLogFactory':create(F, 0, 0).

4. To retrieve the IOR associated to the log manager instance, use::

    corba:object_to_string(F).
