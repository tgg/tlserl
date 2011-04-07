=================
``tlserl`` design
=================

``tlserl`` is built on top of Erlang/OTP. Hence it relies on the following elements:

* worker processes
* supervision tree
* application
* mnesia tables

Worker processes
================

The application is composed of the following worker processes:

a log manager
  this is the implementation of the CORBA ``DsLogAdmin::BasicLogMgr``
  interface. It can be found in ``DsLogAdmin_Factory_impl.erl``.

one or many logs
  this is the implementation of the CORBA ``DsLogAdmin::BasicLog`` interface.
  It can be found in ``DsLogAdmin_Log_impl.erl``.

Supervision tree
================

As of today, there is a single supervision process; it takes care of the log
managers. The supervisor is in ``dsLogAdminSup.erl``. It creates the
``simple_one_for_one`` strategy that will restart log manager processes.

Log processes are started without supervision.

Application
===========

The application is in ``dsLogAdminApp.erl``. 

Important functions
-------------------

``install/0``
~~~~~~~~~~~~~
Installs needed data in the IFR. Not sure really whether it is needed.

``start/0``
~~~~~~~~~~~
Creates the supervisor above.

``start_log_mgr/{0,1}``
~~~~~~~~~~~~~~~~~~~~~~~
This function create (resp reactivate) a log manager. The first time it is
invoked, it will create the ``oe_tlsbf`` table.

For every invocation of ``start_log_mgr/0``, it will create a new log
manager with id ``$i``, as well as table ``oe_tlsbf_$i_log_attr``.

Invoking ``start_log_mgr/1`` skips theses creations.

Mnesia tables
=============
Table ``oe_tlsbf``
------------------
**Purpose**: contains the list of all LogMgr created processes, along with the associated object reference:

**Structure**:

+------------+------------------+
| Factory Id | Object reference |
+------------+------------------+
|            |                  |
+------------+------------------+

Table ``oe_tlsbf_$i_log_attr``
------------------------------
**Purpose**: contains the log ids and attributes for log manager ``$i``:

**Structure**:

+----+-----+------+------+-----+------+-----+-----+-----+-----+----+
| Id | Log | Full | M sz | QoS | M lf | Adm | Fwd | Int | Thr | Wk |
+----+-----+------+------+-----+------+-----+-----+-----+-----+----+
|    |     |      |      |     |      |     |     |     |     |    |
+----+-----+------+------+-----+------+-----+-----+-----+-----+----+

With:

* Id   -- the log id
* Log  -- the log object reference
* Full -- the action to perform when the log is full
* M sz -- the max size of the log
* QoS  -- QoS settings
* M lf -- maximum life of log records
* Adm  -- administrative state of the log
* Fwd  -- forwarding state of the log
* Int  -- log interval
* Thr  -- capacity alarm thresholds
* Wk   -- week mask

Table ``oe_tlsb_$f_$i``
-----------------------
**Purpose**: contains the log records for log ``$i`` and log manager ``$f``:

**Structure**:

Stores directly the ``'DsLogAdmin_LogRecord'`` defined in the IDL.
