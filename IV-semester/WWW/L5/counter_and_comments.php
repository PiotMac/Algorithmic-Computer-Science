<?php    
    function create_footer() { ?>
        <div class="comment_section">
	        <h1>Comment section!</h1>
	        <?php
		    if (isset($_SESSION["isRegistered"]) && $_SESSION["isRegistered"] === true) { ?>
			    <form action="../server.php" method="POST">
				    <input type="text" name="comment_text" placeholder="Write your comment!">
				    <input type="submit" name="comment_submit_main" value="Publish!">
			    </form>
			<?php
				$commentator_ids = $_SESSION["user_ids_1"];
				$comments = $_SESSION["comments_1"];
					
				for ($i = 0; $i < count($commentator_ids); $i++) { ?>
					<h3><?php echo $commentator_ids[$i] ?></h3>
					<p><?php echo $comments[$i] ?></p>	
            <?php
    			}
		    }
		    else { ?>
			    <h3>In order to see the comments register your account!</h3>
	        <?php
    	    }?>
	    </div>
	    <hr class = "rounded">
	    <p class="visitors"> <b>The number of visitors: 
		    <?php
		    	check_visit();
		    ?>
		    </b>
	    </p>
    <?php        
    }
?>
