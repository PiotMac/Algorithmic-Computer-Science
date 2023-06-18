<?php
    function create_nav() { ?>
        <nav>
            <form action="../server.php" method="POST">
                <button type="submit" name="general">General info</button>
                <button type="submit" name="programming">Programming</button>
                <button type="submit" name="equation">Equation</button>
            </form>
		    <?php
			if (isset($_SESSION["isRegistered"]) && $_SESSION["isRegistered"] === true) {
			?>	
				<p class="userInfo"> 
					<?php echo "Logged in as: " . $_SESSION['sessionUsername']; ?>
				</p>
				<form action="../logout.php" method="POST">
  					<button type="submit" name="logout">Log out</button>
				</form>
			<?php	
			} else {
				?>
				<p class="userInfo"> 
					<?php echo "Hey " . $_SESSION['sessionUsername'] . "! You are not logged in."; ?>
				</p>
				<form action="../logout.php" method="POST">
  					<button type="submit" name="logout">Register!</button>
				</form>
			<?php	
			}
		    ?>
	    </nav>
    <?php
    }
?>