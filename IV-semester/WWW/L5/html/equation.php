<?php
	require "../session.php";
	require "../nav.php";
	require "../counter_and_comments.php";
	session_start();
	track_session();
?>

<!DOCTYPE html>
<html lang = "en">
<head>
	<meta charset="UTF-8">
	<link href="../css/styles.css" rel="stylesheet" type="text/css"/>
    <script async src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"></script>
	<title>Equation</title>
	<meta name = "viewport" content = "width = 
			device−width, initial−scale=1.0">
</head>
<body>
    <?php
		create_nav();
	?>
    <h1>Euler Product Formula</h1>
    <p>The Euler Product Formula, named after the Swiss mathematician Leonhard Euler, is a powerful result in number theory that establishes a connection between the values of a function at positive integers and its infinite product representation.</p>
    <p>For a given function <i>f(n)</i> defined on the positive integers and a complex number <i>s</i> with a real part greater than a certain critical value, the Euler Product Formula can be stated as:</p>
    <div>
        <p>\[ \zeta(s) = \sum_{n=1}^{\infty} \frac{f(n)}{n^s} = \prod_{p \, prime} \left(1 + \frac{f(p)}{p^s} + \frac{f(p^2)}{p^{2s}} + \frac{f(p^3)}{p^{3s}} + \ldots\right) = \prod_{p \, prime} \frac{1}{1 - p^{-s}} \]</p>
    </div>
    <p>The left-hand side of the equation represents an infinite series where the function <i>f(n)</i> is evaluated at each positive integer and divided by <i>n^s</i>. The right-hand side of the equation is an infinite product that involves prime numbers (<i>p</i>). It demonstrates that the series can be expressed as the product of terms corresponding to each prime number, where the numerator of each term involves the values of <i>f</i> at powers of the prime number divided by appropriate powers of <i>p</i>.</p>
    <p>The Euler Product Formula has numerous applications in number theory, including the study of zeta functions, Dirichlet series, and prime number distributions.</p>
    <hr class = "rounded">
	<?php
		create_footer();
	?>
</body>
</html>