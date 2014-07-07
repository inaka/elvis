-module(fail_macro_module_names).

-define(FUNCTION_NAME, function_name).
-define(function_name, function_name).
-define(module_name, ?MODULE).

module_name() ->
  ?MODULE:function_name(),
  ?module_name:?function_name().

function_name() ->
  module:?FUNCTION_NAME(params),
  module:?FUNCTION_NAME (params).

no_errors() -> ?LINE.
