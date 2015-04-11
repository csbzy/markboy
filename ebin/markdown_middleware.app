%% Feel free to use, reuse and abuse the code in this file.

{application, markdown_middleware, [
	{description, "Cowboy static file handler example with middleware component."},
	{vsn, "1"},
	{modules, ['dtl_handler', 'echo_get_app', 'erlmarkdown', 'markdown_converter', 'markdown_middleware_app', 'markdown_middleware_sup']},
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
