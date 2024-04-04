package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoginInformationsDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.Objects;
import java.util.Optional;


/**
 * Servlet handling requests for login page.
 */
@WebServlet(name = "LoginServlet", value = "/login")
public class LoginServlet extends HttpServlet {

    @EJB
    private UserEJB userEJB;

    /**
     * Handle a POST request by executing the login procedure.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        System.out.println("@LoginServlet: called doPost() method");

        // Extracting parameters for login
        String username = Optional.ofNullable(request.getParameter("username")).orElse("");
        String password = Optional.ofNullable(request.getParameter("password")).orElse("");

        // Executing login through EJB
        LoggedUserDTO loggedUser = userEJB.login(new LoginInformationsDTO(username, password));

        // Login failed -> sending an 'error' GET parameter
        if (loggedUser == null) {
            // request.setAttribute("loginError", true);
            response.sendRedirect(request.getContextPath() + "/?loginError=error");
            return;
        }

        //Login successful
        AccessController.setLoggedUser(request, Objects.requireNonNull(loggedUser));    // add logged user info to session var
        ClientRedirector.redirectToMainPage(request, response);     // redirect to main page
    }
}
