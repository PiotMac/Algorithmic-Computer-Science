<!DOCTYPE html>
<html>
<head>
    <title>Login and Registration</title>
</head>
<body>
    <h1>Login</h1>
    <form method="POST" action="server.php">
        <input type="text" name="username" placeholder="Username">
        <input type="password" name="password" placeholder="Password">
        <input type="submit" name="login" value="Login">
    </form>

    <h1>Register</h1>
    <form method="POST" action="server.php">
        <input type="text" name="username" placeholder="Username">
        <input type="password" name="password" placeholder="Password">
        <input type="password" name="sec_password" placeholder="Enter again password">
        <input type="submit" name="register" value="Register">
    </form>

    <h1>Guest</h1>
    <form method="POST" action="server.php">
        <input type="submit" name="guest" value="Continue as a Guest!">
    </form>

    <?php
        if (isset($_POST['inactive'])) {
            echo "You have been logged out due to inactivity!"; 
        }
    ?>
</body>
</html>
