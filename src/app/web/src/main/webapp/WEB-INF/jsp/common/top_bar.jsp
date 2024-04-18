<!DOCTYPE html>
<html>
<head>
    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">

    <!-- Link to your CSS file -->
    <link href="${pageContext.request.contextPath}/assets/css/common/top_bar.css" rel="stylesheet">
    <title></title>
</head>
<body>
<div class="navbar">
    <!-- Tools on the left -->
    <div>
        <button class="btn btn-primary" onclick="location.href = '${pageContext.request.contextPath}/homepage'" title="Homepage">Homepage</button>
        <button class="btn btn-primary" onclick="location.href = '${pageContext.request.contextPath}/profile'" title="Profile">Profile</button>
    </div>

    <!-- Additional actions on the right -->
    <div>
        <button class="btn btn-primary" onclick="location.href = '${pageContext.request.contextPath}/logout'" title="Logout">Logout</button>
    </div>
</div>
</body>
</html>
