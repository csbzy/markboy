%% Feel free to use, reuse and abuse the code in this file.

{application, markdown_middleware, [
	{description, "Cowboy static file handler example with middleware component."},
	{vsn, "1"},
	{modules, ['markdown_converter', 'erlmarkdown', 'echo_get_app', 'markdown_middleware_app', 'dtl_handler', 'markdown_middleware_sup', 'home_handler']},
	{registered, [markdown_middleware_sup]},
	{applications, [
		kernel,
		stdlib,
		cowboy,
        erlydtl
	]},
	{mod, {markdown_middleware_app, []}},
	{env, []}
]}.
