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
 </div>

 <script type="text/javascript">
     $(document).ready(function() {
         if ("WebSocket" in window)
         {
             var ws = new WebSocket("ws://172.16.7.119/ws/weixin?uid={{uid}}");
             ws.binaryType = 'arraybuffer';
             ws.onopen = function() {
                 $("#content").append("<p style='color: #80ff00;'>websocket connected!</p>");
             };
             ws.onmessage = function (evt) {

                 $("#content").append("<p>" +  evt.data.byteLength+'\t'  + ab2str(evt.data.slice(0)) + "</p>");
             };
             ws.onclose = function() {
                 $("#content").append("<p style='color: #ff3737;'>websocket closed!</p>");
             };
             $("#msg").change(function() {
                 var val = $(this).val();
                 if ( val )
                 {
                     ws.send(val);
                 }
                 return false;
             });
             $("#smt").click(function() {
                 var val = $("#msg").val();
                 if ( val )
                 {
                     ws.send(val);
                 }
                 return false;
             });
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