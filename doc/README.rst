=========================================
Telecom Log Service Erlang Implementation
=========================================

Telecom Log Service Erlang Implementation (``tlserl``) is an Erlang
implementation of the CORBA Telecom Log Service for the basic log only.

Important object types being used in the basic log service::

                <<creates>>           <<contains>>
  log manager ---------------> logs ◈--------------> log records

* The **log manager** can be used to create, list, find or destroy
  (unimplemented) a **log**
* **Log records** can be added, deleted, and searched for in a given log

For more information on the TelecomLogService, see:
  http://www.omg.org/spec/TLOG/

The project is hosted on Bitbucket:
  https://bitbucket.org/tgg/tlserl/

``tlscli`` is a client that can be used with ``tlserl``:
  https://launchpad.net/tlscli


Prerequisites
=============

To install ``tlserl``, you need an Erlang installation and GNU make. It was
tested with Erlang R14B02.


Installing
==========

We'll assume that ``tlserl`` was unpacked into ``/some/path/tlserl-$V``, where
``$V`` is the version of ``tlserl``.

To compile the software, go to ``src/`` and run ``make``. This will compile all
needed ``.erl`` source files into ``/some/path/tlserl-$V/ebin``.

That's it.


Running
=======

After `Installing`_ the software, you can run Erlang from the
``ebin/`` directory where files were compiled.

First time
~~~~~~~~~~

If you don't have one already, create a Mnesia_ schema::

   1> mnesia:create_schema([node()]).
   ok

And launch Orber_, e.g. using::

   2> orber:jump_start(1234).
   =INFO REPORT==== 7-Apr-2011::22:19:04 ===
   ======= Orber Execution Environment ======
   Orber version.................: 3.6.20
   Orber domain..................: 127.0.1.1:1234
   IIOP port number..............: 1234
   IIOP NAT port number..........: 1234
   Interface(s)..................: ["127.0.1.1"]
   Interface(s) NAT..............: ["127.0.1.1"]
   Local Interface (default).....: []
   Nodes in domain...............: ['nonode@nohost']
   GIOP version (default)........: 1.1
   IIOP out timeout..............: infinity msec
   IIOP out connection timeout...: infinity msec
   IIOP setup connection timeout.: infinity msec
   IIOP out ports................: 0
   IIOP out ports attempts.......: 1
   IIOP out ports random.........: false
   IIOP out connections..........: []
   IIOP out connections (pending): []
   IIOP out keepalive............: false
   IIOP in connections...........: []
   IIOP in connection timeout....: infinity msec
   IIOP in keepalive.............: false
   IIOP max fragments............: infinity
   IIOP max in requests..........: infinity
   IIOP max in connections.......: infinity
   IIOP backlog..................: 5
   IIOP ACL......................: []
   IIOP maximum packet size......: infinity
   Object Keys GC interval.......: infinity
   Using Interceptors............: false
   Using Local Interceptors......: false
   Debug Level...................: 0
   orbInitRef....................: undefined
   orbDefaultInitRef.............: undefined
   System Flags Set..............: -
   =========================================

You can now start the dsLogAdminApp application::

   3> dsLogAdminApp:start().
   ok

and create a new log manager::

   4> F = dsLogAdminApp:start_log_mgr().
   =INFO REPORT==== 7-Apr-2011::22:24:07 ===
   'DsLogAdmin_Factory_impl' initializing with nothing
   {'DsLogAdmin_BasicLogFactory',registered,
                                 oe_dsBasicLogFactory,
                                 <<131,100,0,9,117,110,100,101,102,105,110,101,100>>,
                                 0,0}

After that, ``tlserl`` is ready for use. If you need to communicate the
IOR for a client, you can use ``corba:object_to_string(F).``

Next times
~~~~~~~~~~

Restart mnesia::

   1> mnesia:start().
   ok

as well as orber::

   2> orber:start().
   ok

You can now start the dsLogAdminApp application::

   3> dsLogAdminApp:start().
   ok

To restore the log manager previously created, use::

   4> F = dsLogAdminApp:start_log_mgr(1).
   =INFO REPORT==== 7-Apr-2011::22:24:07 ===
   'DsLogAdmin_Factory_impl' initializing with nothing
   {'DsLogAdmin_BasicLogFactory',registered,
                                 oe_dsBasicLogFactory,
                                 <<131,100,0,9,117,110,100,101,102,105,110,101,100>>,
                                 0,0}

The ``1`` is the ID of the first log manager created.

After that, ``tlserl`` is ready for use. Again, if you need to communicate the
IOR for a client, you can use ``corba:object_to_string(F).``

.. _Orber: http://www.erlang.org/doc/man/orber.html
.. _Mnesia: http://www.erlang.org/doc/man/mnesia.html
