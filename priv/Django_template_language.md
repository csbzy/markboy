Templates¶

A template is simply a text file. It can generate any text-based format (HTML, XML, CSV, etc.).
一个模版是一个普通的text文件，它会被转化成任何的文本格式（html，xml，csv）
A template contains variables, which get replaced with values when the template is evaluated, and tags, which control the logic of the template.
模版中包含了变量，这些变量会在模版被转义时替换（标签也一样会被转义）


Variables¶

Variables look like this: {{ variable }}. When the template engine encounters a variable, it evaluates that variable and replaces it with the result.
变量形似：{{variable}},当模版引擎遇到变量是，这个变量会被计算和替换为结果。
Variable names consist of any combination of alphanumeric characters and the underscore ("_").
变量名称由字母数字字符和“—”组成。

dot
The dot (".") also appears in variable sections, although that has a special meaning, as indicated below. Importantly, you cannot have spaces or punctuation characters in variable names.
"."也可以出现在变量中，但是它有特殊的含义。
".":用来表示查找，包括：Dictionary lookup；Attribute lookup；Method call；List-index lookup
In the above example, {{ section.title }} will be replaced with the title attribute of the section object.

Filters¶

通过filters，你可以修改变量的显示方式。
filter是这样使用的：{{ name|lower }}. 通过管道“|”来触发filters（Use a pipe (|) to apply a filter）
内建的filter语句：
add:{{value|add:"2"}}
addslashes:{{value|addslashes}}，加斜杠
capfirst:{{value|capfirst}},大小





Argument	Outputs
openblock	{%
closeblock	%}
openvariable	{{
closevariable	}}
openbrace	{
closebrace	}
opencomment	{#
closecomment	#}












django 模版语言的内建标签
autuescape
控制是否使用自动转义功能，对某一block使用，这个block由endautoescape结束标签
block
定义一个块，这个块可以被子模版继承。
comment
注释的作用？，同时comment无法嵌套使用
extends
标识这个模版有一个父模版，{% extends xxx %}
firstof
输出第一个不是false的变量，{% firstof var1 var2 var3%}
now
显示当前的日期
if
{% if xxx %}  .... {%elif  xxx %}  ....  {%else%}  .... {%endif%} 
cycle循环
｛% for 0 in some_list %}    ....   {%endfor%} 