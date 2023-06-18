<?php

function begin_tracking_session() {
    $_SESSION['last_activity'] = time();
}

function track_session() {
    $inactive_timeout = 300;

    if (isset($_SESSION['last_activity']) && time() - $_SESSION['last_activity'] > $inactive_timeout) {
        return true;
        //destroy_session();
    }
    else {
        $_SESSION['last_activity'] = time();
        return false;
    }
}

function destroy_session() {
    session_unset();
    session_destroy();
    header("Location: logout.php");
    //header("Location: ../WWW_L5/indextest.php");
    exit();
}

function check_visit() {
    $ip = $_SERVER['REMOTE_ADDR'];
    //$ip = '192.158.1.39';
    $lastVisit = getLastVisit($ip);
    $currentTimestamp = time(); // Get the current Unix timestamp
    $count_file = '/var/www/html/WWW_L5/visit_count.txt';
    $number = file_get_contents($count_file);
    if ($lastVisit === null || $currentTimestamp - $lastVisit >= 86400) {
        $number++;
        file_put_contents($count_file, $number);

        echo $number;

        updateLastVisit($ip);
    }
    else {
        echo $number;
    }
}

function updateLastVisit($ip) {
    $visits_file = '/var/www/html/WWW_L5/visits.txt';
    $visits = json_decode(file_get_contents($visits_file), true);
    $visits[$ip] = time();
    file_put_contents($visits_file, json_encode($visits), LOCK_EX);
}

function getLastVisit($ip) {
    $visits_file = '/var/www/html/WWW_L5/visits.txt';
    $data = file_get_contents($visits_file);
    $visits = json_decode($data, true);

    if (isset($visits[$ip])) {
        return $visits[$ip];
    }

    return null;
}

?>

