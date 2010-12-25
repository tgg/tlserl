IDL := $(wildcard idl/*.idl)

all:
	erlc -I idl -o src +'this' +'{{impl, "DsLogAdmin::Log"}, "DsLogAdmin_Log_impl"}' +'{{impl, "DsLogAdmin::BasicLog"}, "DsLogAdmin_Log_impl"}' +'{{impl, "DsLogAdmin::BasicLogFactory"}, "DsLogAdmin_Factory_impl"}' +'{{impl, "DsLogAdmin::LogMgr"}, "DsLogAdmin_Factory_impl"}' $(IDL)

impl:
	erlc -I idl -o generated +'this' +"{be,erl_template}" +'{{impl, "DsLogAdmin::Log"}, "DsLogAdmin_Log_impl"}' +'{{impl, "DsLogAdmin::BasicLog"}, "DsLogAdmin_Log_impl"}' +'{{impl, "DsLogAdmin::BasicLogFactory"}, "DsLogAdmin_Factory_impl"}' +'{{impl, "DsLogAdmin::LogMgr"}, "DsLogAdmin_Factory_impl"}' $(IDL)
