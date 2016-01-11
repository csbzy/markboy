<!DOCTYPE html>
<html>
<head>
	<script type="text/javascript" src="/js/jquery.min.js"></script>
	<script type="text/javascript" src="/js/bootstrap.min.js"></script>
    <style type='text/css'>
        body {
            background-color: #CCC;
        }
    </style>
</head>
<body>

<div class="container ">
  <img src = "{{ img }}" >
  <p id ="content" class = "content"></p>
 </div>

 <script type="text/javascript">
     $(document).ready(function() {
         if ("WebSocket" in window)
         {
             var ws = new WebSocket("ws://172.16.7.119/ws/weixin?uid={{uid}}");
             ws.binaryType = 'arraybuffer';
             ws.onopen = function() {
                 console.log("open ws socket");
             };
             ws.onmessage = function (evt) {
                 console.log(evt);
                 $("#content").append("<p>" +  evt.data + "</p>");
             };
             ws.onclose = function() {
                 $("#content").append("<p style='color: #ff3737;'>websocket closed!</p>");
             };

         }
         else
         {
             $("#content").append("<p style='color: #ff3737;'>Your browser don't support WebSocket!</p>");
         };
     });
     function ab2str(buf) {
         return String.fromCharCode.apply(null, new Uint8Array(buf));
     }
 </script>
</body>

</html>