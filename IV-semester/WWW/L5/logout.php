<?php
//session_start();

if (isset($_POST['logout'])) {
  session_unset();
  session_destroy();
}

?>

<!DOCTYPE html>
<html lang = "en">
<head>
	<meta charset="UTF-8">
	<link href="../css/styles.css" rel="stylesheet" type="text/css"/>
	<title>Logged out!</title>
	<meta name = "viewport" content = "width = 
			device−width, initial−scale=1.0">
</head>
<body>
  <p>You have been logged out!</p>
  <button onclick="window.location.href='indextest.php';">Go back to login page!</button>
</body>
