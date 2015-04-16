{% extends "base.tpl" %}
{% block content %}
<div class="well" >
   {% markdown path %}
</div>
<div id="uyan_frame" class="well"></div>
<script type="text/javascript" src="http://v2.uyan.cc/code/uyan.js"></script>
{% endblock  %}
