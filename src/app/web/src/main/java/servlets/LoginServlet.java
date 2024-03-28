package servlets;


import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoginInformationsDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.Optional;

/**
 * Servlet handling POST requests for login.
 */
@WebServlet(name = "LoginServlet", value = "/login")
public class LoginServlet extends HttpServlet {

    public static final String failedLoginErrorMessage = "Wrong username or password";

    @EJB
    private UserEJB userEJB;

    /**
     * Handle a POST request by executing the login procedure.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
        // Extract parameters for login
        String username = Optional.ofNullable(request.getParameter("username")).orElse("");
        String password = Optional.ofNullable(request.getParameter("password")).orElse("");

        // Execute login through EJB
        LoggedUserDTO loggedUser = userEJB.login(
            new LoginInformationsDTO(username, password)
        );

        // Login failed
        if (loggedUser == null) {
            response.sendRedirect(request.getContextPath() + "/index.jsp?param=error");
        }

        //Login successful
        AccessController.setLoggedUser(request, loggedUser);    // add logged user info to session var
        ClientRedirector.redirectToMainPage(request, response);
    }

}
