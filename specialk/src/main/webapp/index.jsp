<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<html>
	<head>
		<style>
			body {
				background-color: #eef0f6;
				margin: 0px;
				font-size: .9em;
			}
			
			a, a:hover, a:visited { color:black; text-decoration:none; }
			a, a:visited { border-bottom: 1px dotted black; }
			a:hover { border-bottom: 1px solid black; }
				
			#content { 
				background-color: white; 
				margin: 0px 300px 0px 20px;
				padding: 20px;
				border-top: 1px solid #6A829E;
				border-left: 1px solid #6A829E;
				border-right: 1px solid #6A829E;
				border-bottom: 1px solid #6A829E;
			}
			h2.title { margin: 0px; padding: 20px;}
			
			#sidebar {
				position: absolute;
				right: 10px; top: 60px;
				width: 250px;
				padding: 0 10px 10px 10px;				
			}
			#sidebar h3 { margin: 10px 0px 5px 0px; }
			#sidebar p { margin: 10px 5px 10px 5px;}
			#sidebar li { list-style: none; margin: 0px; padding: 5px 0px 5px 0px;}
			#sidebar ul { margin: 5px; padding: 0px;}			
			#sidebar li { padding: 5px 0px 5px 0px;}			
			#sidebar table { width: 160px; margin: 5px;}			
			#sidebar td { border-bottom: 1px solid #6A829E; padding: 2px 5px 2px 5px;}
			#sidebar .col2 { text-align: right; }
		</style>
	</head>
<body>
	<div id="sidebar">
		<h3>Getting started</h3>
		<p>
		Read <a href="http://wiki.stax.net/w/index.php/GettingStarted/ScalaMaven">this tutorial</a> to learn how to build and deploy this application locally.
		</p>
		<h3>Stax links</h3>
		<ul>
			<li><a href="http://www.stax.net/appconsole">Stax Web Console</a></li>
			<li><a href="http://wiki.stax.net/w/index.php/Main_Page">Stax Developer Wiki</a></li>
		</ul>		
		<h3>Runtime information</h3>
		<table cellpadding="0" cellspacing="0">
			<tr><td class="col1">Environment:</td><td class="col2"><%=pageContext.getServletContext().getInitParameter("application.environment") %></td></tr>
			<tr><td class="col1">Server:</td><td class="col2"><%= java.net.InetAddress.getLocalHost().getHostName() %></td></tr>
		</table>
	</div>
	<h2 class="title">This application is still under development</h2>
	<div id="content">	
		<h3>Your Scala web application is running</h3>
		<p><a href="/hello">Click here to see a Scala Servlet</a></p>
	</div>
</body>
</html>
