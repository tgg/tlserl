#
# %CopyrightBegin%
# 
# Copyright © 2011, Thomas Girard <thomas.g.girard@free.fr>
# All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
#
#
# NOTE: This Makefile is directly inspired from Erlang ones.
#

include ../vsn.mk
VSN=$(DSLOGADMIN_VSN)

EBIN = ../ebin
EMULATOR = beam
ERL_COMPILE_FLAGS = +debug_info
ERLC_WFLAGS = -W
ERLC = erlc $(ERLC_WFLAGS) $(ERLC_FLAGS)

EXTERNAL_INC_PATH = ../include

MODULES = \
	DsLogAdmin_Common \
	DsLogAdmin_Factory_impl \
	DsLogAdmin_Iterator_impl \
	DsLogAdmin_Log_impl \
	dsLogAdminApp \
	dsLogAdminSup

ERL_FILES = $(MODULE:%=.erl)

GEN_DSLOGADMIN_ERL_FILES = \
	DsLogAdmin.erl \
	DsLogAdmin_Anys.erl \
	DsLogAdmin_AvailabilityStatus.erl \
	DsLogAdmin_BasicLog.erl \
	DsLogAdmin_BasicLogFactory.erl \
	DsLogAdmin_CapacityAlarmThresholdList.erl \
	DsLogAdmin_IntervalsOfDay.erl \
	DsLogAdmin_InvalidAttribute.erl \
	DsLogAdmin_InvalidConstraint.erl \
	DsLogAdmin_InvalidGrammar.erl \
	DsLogAdmin_InvalidLogFullAction.erl \
	DsLogAdmin_InvalidMask.erl \
	DsLogAdmin_InvalidParam.erl \
	DsLogAdmin_InvalidRecordId.erl \
	DsLogAdmin_InvalidThreshold.erl \
	DsLogAdmin_InvalidTime.erl \
	DsLogAdmin_InvalidTimeInterval.erl \
	DsLogAdmin_Iterator.erl \
	DsLogAdmin_Log.erl \
	DsLogAdmin_LogDisabled.erl \
	DsLogAdmin_LogFull.erl \
	DsLogAdmin_LogIdAlreadyExists.erl \
	DsLogAdmin_LogIdList.erl \
	DsLogAdmin_LogList.erl \
	DsLogAdmin_LogLocked.erl \
	DsLogAdmin_LogMgr.erl \
	DsLogAdmin_LogOffDuty.erl \
	DsLogAdmin_LogRecord.erl \
	DsLogAdmin_NVList.erl \
	DsLogAdmin_NVPair.erl \
	DsLogAdmin_QoSList.erl \
	DsLogAdmin_RecordIdList.erl \
	DsLogAdmin_RecordList.erl \
	DsLogAdmin_Time24.erl \
	DsLogAdmin_Time24Interval.erl \
	DsLogAdmin_TimeInterval.erl \
	DsLogAdmin_UnsupportedQoS.erl \
	DsLogAdmin_WeekMask.erl \
	DsLogAdmin_WeekMaskItem.erl \
	oe_DsLogAdmin.erl

GEN_DSLOGADMIN_HRL_FILES = \
	DsLogAdmin.hrl \
	DsLogAdmin_BasicLog.hrl \
	DsLogAdmin_BasicLogFactory.hrl \
	DsLogAdmin_Iterator.hrl \
	DsLogAdmin_Log.hrl \
	DsLogAdmin_LogMgr.hrl \
	oe_DsLogAdmin.hrl

EXTERNAL_DSLOGADMIN_HRL_FILES = $(GEN_DSLOGADMIN_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)

GEN_TIMEBASE_ERL_FILES = \
	TimeBase_UtcT.erl \
	TimeBase_IntervalT.erl \
	oe_TimeBase.erl

GEN_TIMEBASE_HRL_FILES = \
	TimeBase.hrl \
	oe_TimeBase.hrl

EXTERNAL_TIMEBASE_HRL_FILES = $(GEN_TIMEBASE_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)

TARGET_FILES = \
	$(GEN_DSLOGADMIN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

APPUP_FILE = dsLogAdmin.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

APP_FILE = dsLogAdmin.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

ERL_COMPILE_FLAGS += \
	-I../include

opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

clean:
	rm -Rf $(EXTERNAL_DSLOGADMIN_HRL_FILES) $(EXTERNAL_TIMEBASE_HRL_FILES) $(EBIN)/* *~ $(GEN_DSLOGADMIN_ERL_FILES)

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(GEN_DSLOGADMIN_ERL_FILES) $(EXTERNAL_DSLOGADMIN_HRL_FILES): DsLogAdmin.idl $(EXTERNAL_TIMEBASE_HRL_FILES)
	erlc -I . +'this' +'{{impl, "DsLogAdmin::Log"}, "DsLogAdmin_Log_impl"}' +'{{impl, "DsLogAdmin::BasicLog"}, "DsLogAdmin_Log_impl"}' +'{{impl, "DsLogAdmin::BasicLogFactory"}, "DsLogAdmin_Factory_impl"}' +'{{impl, "DsLogAdmin::LogMgr"}, "DsLogAdmin_Factory_impl"}' $<
	mv $(GEN_DSLOGADMIN_HRL_FILES) $(EXTERNAL_INC_PATH)

$(EXTERNAL_TIMEBASE_HRL_FILES): TimeBase.idl
	erlc $<
	mv $(GEN_TIMEBASE_HRL_FILES) $(EXTERNAL_INC_PATH)
	rm $(GEN_TIMEBASE_ERL_FILES)

$(EBIN)/%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

dist:
	hg archive --prefix=tlserl-$(DSLOGADMIN_VSN) ../../tlserl-$(DSLOGADMIN_VSN).tar.gz

.PHONY: clean opt dist
