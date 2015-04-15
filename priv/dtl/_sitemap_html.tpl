


<h3>文章列表</h3>
<ol>
{% for path,tittle,act_date in result %}
<li><a href={{ path }}  title="{{ tittle }}" target="_blank">{{ tittle }} ------------ {{ act_date }}</a></li>
{% endfor %}
</ol>
