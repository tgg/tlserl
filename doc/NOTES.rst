======
tlserl
======

``tlserl`` is a basic CORBA Telecom Log Service implementation
written in Erlang.

RUNNING
=======

High level
----------

Creating a log factory and a log
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Create a mnesia schema::

   mnesia:create_schema([node()]).

2. Launch orber::

   orber:jump_start(1234).

3. [OPTIONAL?] Install dsLogAdminApp into the IFR using::

   dsLogAdminApp:install().

4. Start the dsLogAdminApp application::

   dsLogAdminApp:start().

5. Create a new log factory::

   F = dsLogAdminApp:start_log_mgr().

6. To retrieve the IOR associated to the log manager instance, use::

   corba:object_to_string(F).

7. [OPTIONAL] To create a new log using this factory::

   {L,Id}='DsLogAdmin_BasicLogFactory':create(F, 0, 0).


Resuming a session
~~~~~~~~~~~~~~~~~~
1. Restart mnesia::

   mnesia:start().

2. Restart orber::

   orber:start().

3. [OPTIONAL?] Install dsLogAdminApp into the IFR using::

   dsLogAdminApp:install().

4. Start the dsLogAdminApp application::

   dsLogAdminApp:start().

5. Reuse previous log factory (id defaults to 1)::

   F = dsLogAdminApp:start_log_mgr(1).

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
