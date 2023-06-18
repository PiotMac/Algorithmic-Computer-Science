<?php

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);


require "session.php";

$servername = 'localhost';
$usernameDB = 'root';
$passwordDB = '';
$database = 'users';

// Create a new mysqli object
$conn = new mysqli($servername, $usernameDB, $passwordDB, $database);

// Check the connection
if ($conn->connect_error) {
    die('Connection failed: ' . $conn->connect_error);
}

// Check if the form is submitted for registration
if (isset($_SERVER['REQUEST_METHOD']) && $_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['register'])) {
    $username = $_POST['username'] ?? '';
    $password = $_POST['password'] ?? '';
    $sec_password = $_POST['sec_password'] ?? '';

    if ($password != $sec_password) {
        echo 'Error: Two entered passwords are different!';
    }
    else {
        $query = "SELECT * FROM Users WHERE username = '$username'";
        $result = $conn->query($query);

        if ($result->num_rows > 0) {
            echo 'Error: Username already exists.';
        } 
        else {
            // Password requirements
            $uppercase = preg_match('@[A-Z]@', $password);
            $lowercase = preg_match('@[a-z]@', $password);
            $number = preg_match('@[0-9]@', $password);
            $specialChars = preg_match('@[^\w]@', $password);

            // Username requirements
            if (strlen($username) < 6) {
                echo 'Error: Username must have at least 6 letters.';
            }
            else if (!$uppercase || !$lowercase || !$number || !$specialChars || strlen($password) > 15) {
                echo 'Error: Password must have at least one uppercase letter, one lowercase letter, one digit, one special character, and cannot exceed 15 characters.';
            }
            else {
                // Insert the registration data into the users table
                $password = password_hash($password, PASSWORD_DEFAULT);
                $query = "INSERT INTO Users (username, password) VALUES ('$username', '$password')";

                if ($conn->query($query) === TRUE) {
                    session_start();
                    $isRegisteredUser = true;
                    $sessionUsername = $username;
                    $_SESSION["isRegistered"] = $isRegisteredUser;
                    $_SESSION["sessionUsername"] = $sessionUsername;


                    $comments_array_query = "SELECT * FROM Comments WHERE section_id = '1'";
                    $comments_array_main = $conn->query($comments_array_query);
                    $comments_array_query = "SELECT * FROM Comments WHERE section_id = '2'";
                    $comments_array_programming = $conn->query($comments_array_query);

                    $user_ids_1 = array();
                    $comments_1 = array();
                    $user_ids_2 = array();
                    $comments_2 = array();
                    while ($row = $comments_array_main->fetch_assoc()) {
                        $user_ids_1[] = $row['user_id'];
                        $comments_1[] = $row['comment'];
                    }
                    while ($row = $comments_array_programming->fetch_assoc()) {
                        $user_ids_2[] = $row['user_id'];
                        $comments_2[] = $row['comment'];
                    }

                    $_SESSION["user_ids_1"];
                    $_SESSION["comments_1"];
                    $_SESSION["user_ids_2"];
                    $_SESSION["comments_2"];


                    header("Location: html/general.php");
                    exit();
                } else {
                    echo 'Error: ' . $conn->error;
                }
            }
        }
    }
}

// Check if the form is submitted for login
if (isset($_SERVER['REQUEST_METHOD']) && $_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['login'])) {
    $username = $_POST['username'] ?? '';
    $password = $_POST['password'] ?? '';

    // Retrieve the user's record from the database
    $query = "SELECT * FROM Users WHERE username = '$username'";
    $result = $conn->query($query);

    if ($result->num_rows === 1) {
        $row = $result->fetch_assoc();
        $storedHashedPassword = $row['password'];

        // Verify the input password against the stored hashed password
        if (password_verify($password, $storedHashedPassword)) {
            session_start();
            $isRegisteredUser = true;
            $sessionUsername = $username;
            $_SESSION["isRegistered"] = $isRegisteredUser;
            $_SESSION["sessionUsername"] = $sessionUsername;

            $comments_array_query = "SELECT * FROM Comments WHERE section_id = '1'";
            $comments_array_main = $conn->query($comments_array_query);
            $comments_array_query = "SELECT * FROM Comments WHERE section_id = '2'";
            $comments_array_programming = $conn->query($comments_array_query);
            $comments_array_query = "SELECT * FROM Comments WHERE section_id = '3'";
            $comments_array_equation = $conn->query($comments_array_query);

            $usernames1 = array();
            $usernames2 = array();
            $comments1 = array();
            $comments2 = array();
            $usernames3 = array();
            $comments3 = array();

            while ($row = $comments_array_main->fetch_assoc()) {
                $user_id = $row['user_id'];
                $comments1[] = $row['comment'];
            
                // Retrieve the username from the Users table based on the user ID
                $get_username_query = "SELECT username FROM Users WHERE user_id = '$user_id'";
                $result = $conn->query($get_username_query);
                $username_row = $result->fetch_assoc();
            
                // Store the retrieved username in the array
                $usernames1[] = $username_row['username'];
            }
            while ($row = $comments_array_programming->fetch_assoc()) {
                $user_id = $row['user_id'];
                $comments2[] = $row['comment'];
            
                // Retrieve the username from the Users table based on the user ID
                $get_username_query = "SELECT username FROM Users WHERE user_id = '$user_id'";
                $result = $conn->query($get_username_query);
                $username_row = $result->fetch_assoc();
            
                // Store the retrieved username in the array
                $usernames2[] = $username_row['username'];
            }
            while ($row = $comments_array_equation->fetch_assoc()) {
                $user_id = $row['user_id'];
                $comments3[] = $row['comment']; 
            
                // Retrieve the username from the Users table based on the user ID
                $get_username_query = "SELECT username FROM Users WHERE user_id = '$user_id'";
                $result = $conn->query($get_username_query);
                $username_row = $result->fetch_assoc();
            
                // Store the retrieved username in the array
                $usernames3[] = $username_row['username'];
            }

            $_SESSION["user_ids_1"] = $usernames1;
            $_SESSION["comments_1"] = $comments1;
            $_SESSION["user_ids_2"] = $usernames2;
            $_SESSION["comments_2"] = $comments2;
            $_SESSION["user_ids_3"] = $usernames3;
            $_SESSION["comments_3"] = $comments3;

            header("Location: html/general.php");
            exit();
        } else { ?>
            <p>
                <?php echo 'Error: Incorrect password.'; ?>
            </p>   
        <?php     
        }
    } else { ?>
        <p>
            <?php echo 'Error: User does not exist.'; ?>
        </p>
    <?php      
    }
}

// Page for not logged in users
if (isset($_SERVER['REQUEST_METHOD']) && $_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['guest'])) {
    session_start();

    $count_file = '/var/www/html/WWW_L5/visit_count.txt';
    $number = file_get_contents($count_file);
    $ip = $_SERVER['REMOTE_ADDR'];
    $lastVisit = getLastVisit($ip);
    $currentTimestamp = time();

    if ($lastVisit === null || $currentTimestamp - $lastVisit >= 86400) {
        $number++;
        file_put_contents($count_file, $number);

        updateLastVisit($ip);
    }


    $isRegisteredUser = false;
    $sessionUsername = "Guest" . $number;
    $_SESSION["isRegistered"] = $isRegisteredUser;
    $_SESSION["sessionUsername"] = $sessionUsername;
    header("Location: html/general.php");
    exit();
}

if (isset($_SERVER['REQUEST_METHOD']) && $_SERVER['REQUEST_METHOD'] === 'POST' && (isset($_POST['comment_submit_main']) || isset($_POST['comment_submit_programming']) || isset($_POST['comment_submit_equation']))) {
    session_start();
    $isDestroyed = track_session();
    if ($isDestroyed) {
        destroy_session();
    }
    else {
        $username = $_SESSION['sessionUsername'];
        $comment_text = mysqli_real_escape_string($conn, $_POST["comment_text"]);

        $get_user_id_query = "SELECT user_id FROM Users WHERE username = '$username'";
        $result = $conn->query($get_user_id_query);
        $row = $result->fetch_assoc();
        $user_id = $row['user_id'];

        if (isset($_POST['comment_submit_main'])) {
            $query = "INSERT INTO Comments (user_id, section_id, comment) VALUES ('$user_id', '1', '$comment_text')";
            $conn->query($query);

            $updateCommentUsernames = $_SESSION["user_ids_1"];
            $updateCommentUsernames[] = $username;
            $_SESSION["user_ids_1"] = $updateCommentUsernames;

            $updateCommentTexts = $_SESSION["comments_1"];
            $updateCommentTexts[] = $comment_text;
            $_SESSION["comments_1"] = $updateCommentTexts;

            header("Location: html/general.php");
        }
        else if ($_POST['comment_submit_programming']) {
            $query = "INSERT INTO Comments (user_id, section_id, comment) VALUES ('$user_id', '2', '$comment_text')";
            $conn->query($query);

            $updateCommentUsernames = $_SESSION["user_ids_2"];
            $updateCommentUsernames[] = $username;
            $_SESSION["user_ids_2"] = $updateCommentUsernames;

            $updateCommentTexts = $_SESSION["comments_2"];
            $updateCommentTexts[] = $comment_text;
            $_SESSION["comments_2"] = $updateCommentTexts;

            header("Location: html/programming.php");
        }
        else {
            $query = "INSERT INTO Comments (user_id, section_id, comment) VALUES ('$user_id', '3', '$comment_text')";
            $conn->query($query);

            $updateCommentUsernames = $_SESSION["user_ids_3"];
            $updateCommentUsernames[] = $username;
            $_SESSION["user_ids_3"] = $updateCommentUsernames;

            $updateCommentTexts = $_SESSION["comments_3"];
            $updateCommentTexts[] = $comment_text;
            $_SESSION["comments_3"] = $updateCommentTexts;

            header("Location: html/equation.php");
        }
        exit();
    }
}

if (isset($_SERVER['REQUEST_METHOD']) && $_SERVER['REQUEST_METHOD'] === 'POST' && (isset($_POST['general']) || isset($_POST['programming']) || isset($_POST['equation']))) {
    session_start();
    $isDestroyed = track_session();
    if ($isDestroyed) {
        destroy_session();
    }
    else {
        if (isset($_POST['general'])) {
            header("Location: html/general.php");
        }
        else if (isset($_POST['programming'])) {
            header("Location: html/programming.php");
        }
        else {
            header("Location: html/equation.php");
        }
        exit();
    }
}


if (isset($_SERVER['REQUEST_METHOD']) && $_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['logout'])) {
    session_start();
    destroy_session();
}


// Close the database connection
$conn->close();
?>
