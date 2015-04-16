<!DOCTYPE html>
<html>
<head>
	<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <title>csbzy's blog </title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <link rel="stylesheet" href="css/bootstrap.min.css"/>
	<link rel="stylesheet" href="css/sh99.css"/>
	<script type="text/javascript" src="/js/jquery.min.js"></script>
	<script type="text/javascript" src="/js/bootstrap.min.js"></script>
    <style type='text/css'>
        body {
            background-color: #CCC;
        }
    </style>
</head>
<body>
<header class="container ">
    <h1 style="text-align:center">CSB.ZY </h1>
    <h4 style="text-align:center"> In memory of
    memory </h4>
</header>

<div class="container ">
    <nav class="navbar navbar-default">
    	<div class="navbar-header">
    	  <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#bs-example-navbar-collapse-2" aria-expanded="true">
                <span class="sr-only">Toggle navigation</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
              </button>
    	  <a class="navbar-brand" href="#">CSB.ZY</a>
    	</div>
    	<div class="navbar-collapse collapse in" id="bs-example-navbar-collapse-2" aria-expanded="true">
    	<ul class="nav navbar-nav">
        <ul id="navigation" class="nav navbar-nav">
                    <li class="">
                        <a href="/" class="page_home" data-target="#">
                            HOME
                        </a>
                    </li>
                    <li class="">
                        <a href="/page/313/about" class="page_about" data-target="#">
                            ABOUT
                        </a>
                    </li>
                    <li class="">
                        <a href="/page/361/sitemap" class="sitemap" data-target="#">
                            SiteMap
                        </a>
                    </li>
        </ul>
        </ul>
	</div>
	</nav>
</div>
<div class="container">
    <div class=" col-lg-12 col-md-12">
        {% block content %}
        {% endblock %}
    </div>

</div>

<div class="row">
    <div class="col-lg-12 col-md-12 clearfix" id="footer">

        <p style="text-align:center"> 前端<a href="http://http://getbootstrap.com/">Bootstrap</a>&nbsp;|&nbsp;后端<a
                href="http://">Cowboy + erlydtl + markdown</a> 搭建 &nbsp;|&nbsp;托管在阿里 </p>
    </div>
</div>
</body>
{% include "_html_body.tpl" %}
</html>
