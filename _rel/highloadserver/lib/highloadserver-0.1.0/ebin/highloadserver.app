{application, 'highloadserver', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['highloadserver_app','highloadserver_sup','http_protocol']},
	{registered, [highloadserver_sup]},
	{applications, [kernel,stdlib,ranch]},
	{mod, {highloadserver_app, []}},
	{env, []}
]}.