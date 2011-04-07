======
DESIGN
======

``tlserl`` is built on top of OTP. Hence it relies on the following elements:
* worker processes
* supervision tree
* application

Worker processes
================

The application is composed of the following processes:
* a log manager, implementing ``DsLogAdmin::BasicLogMgr``
* one or many logs, implementing ``DsLogAdmin::BasicLog``


Supervision tree
================


Application
===========
