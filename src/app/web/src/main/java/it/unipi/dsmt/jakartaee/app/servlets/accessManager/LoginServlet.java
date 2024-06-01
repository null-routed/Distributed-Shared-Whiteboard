package it.unipi.dsmt.jakartaee.app.servlets.accessManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoginInformationsDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.JWT;
import jakarta.ejb.EJB;
import jakarta.servlet.RequestDispatcher;
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
        if(AccessController.getLoggedUser(request) != null) {
            ClientRedirector.redirectToMainPage(request, response);
            return;
        }
        System.out.println("@LoginServlet: called doPost() method");

        // Extracting parameters for login
        String username = Optional.ofNullable(request.getParameter("username")).orElse("");
        String password = Optional.ofNullable(request.getParameter("password")).orElse("");

        // Executing login through EJB
        LoggedUserDTO loggedUser = userEJB.login(new LoginInformationsDTO(username, password));

        // Login failed -> sending an 'error' GET parameter
        if (loggedUser == null) {
            request.setAttribute("loginError", true);
            RequestDispatcher dispatcher = request.getRequestDispatcher("");
            dispatcher.forward(request, response);
            return;
        }

        //Login successful
        // Generate JWT token and set it as a cookie in the response
        JWT.generateTokenAndSetCookie(response, loggedUser.getUsername());

        // add logged user info to session var
        AccessController.setLoggedUser(request, Objects.requireNonNull(loggedUser));
        response.getWriter().write("InitiateWebSocketConnection");

        ClientRedirector.redirectToMainPage(request, response);     // redirect to main page
    }
}
