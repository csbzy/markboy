
路由机制
配置链接URL到Erlang模块（处理对应请求的模块）的映射就是路由。

当 Cowboy接收到一个请求时，它会尝试在给定的分发规则中匹配 域名 到对应的资源路径。如果匹配成功，那么对应的Erlang 模块就会被执行。

Cowboy首先会匹配域名，接着尝试找到匹配的资源路径。

Cowboy会将路由规则编译后，再使用。


结构
通用的路由结构是这样定义的：
Routes= [Host1, Host2, ... HostN].

每一个域名的匹配规则 包含了 对连接路径的选项约束和路径部件的列表。

Host1= {HostMatch, PathsList}.
Host2= {HostMatch, Constraints, PathsList}.

路径部件的定义如下：
PathsList= [Path1, Path2, ... PathN].

最后，每一路径包含了该路径自身的匹配规则以及对应的处理模块
Path1= {PathMatch, Handler, Opts}.
Path2= {PathMatch, Constraints, Handler, Opts}.

匹配规则语法
匹配语法规则用于标识 域名到路径间的处理handlers。
域名的语法规则：
HostMatch1= "cowboy.example.org".
HostMatch2= "cowboy.example.org.".
HostMatch3= ".cowboy.example.org".

路径的语法规则：
PathMatch= "/hats/:name/prices".
HostMatch= ":subdomain.example.org".

此外，还可以将域名的某一字段保存到Req对象中，再后续可以使用，这就是值绑定。
PathMatch= "/hats/:name/prices".
HostMatch= ":subdomain.example.org".
比如，http://test.example.org/hats/wild_cowboy_legendary/prices 将会
把test绑定到subdomain，而wild_cowboy_legendary就会绑定到name，它们可以被cow_req:binding/{2,3}中检索，绑定的名字必须是一个atom
'_'：表示匹配任何内容

约束
匹配域名和路径完成后，就会检测是否满足可选的约束，约束如下：
{Name, int}
{Name, function, fun ((Value) -> true | {true, NewValue} | false)}
int 约束将会检查 绑定的二进制串是一个int，或可被转化成一个int
function约束，将会调用给定的约束函数并且返回结果，给定函数必须自己保证不会崩溃的。


为了保存Cowboy可以更高效地查找正确的handler模块，Cowboy会编译定义好的路由分发规则。
编译的方法是：cowboy_router:compile/1.
Dispatch= cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, Opts})}
    {'_', [{'_', my_handler, []}]}
]),
%% Name, NbAcceptors, TransOpts, ProtoOpts
cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
).

如果定义好的路由分发规则有错误，cowboy_router:compile/1 
将会返回{error, badarg}

在线更新路由规则
通过 cowboy:set_env/3更新路由的分发规则，连接监听模块接受新的连接时就会使用新的路由分发规则。
cowboy:set_env(my_http_listener, dispatch,
    cowboy_router:compile(Dispatch)).



