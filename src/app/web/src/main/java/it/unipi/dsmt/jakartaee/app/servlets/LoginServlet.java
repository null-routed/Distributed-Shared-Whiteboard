package it.unipi.dsmt.jakartaee.app.servlets;


import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoginInformationsDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

/**
 * Servlet handling POST requests for login.
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
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("LoginServlet: called 'doPost' method...");

        // Extract parameters for login
        String username = Optional.ofNullable(request.getParameter("username")).orElse("");
        String password = Optional.ofNullable(request.getParameter("password")).orElse("");

        // Execute login through EJB
        LoginInformationsDTO loginInformationsDTO = new LoginInformationsDTO(username, password);
        LoggedUserDTO loggedUser = userEJB.login(loginInformationsDTO);

        System.out.println("LoginServlet: login procedure completed");

        // Login failed -> sending an 'error' GET parameter
        if (loggedUser == null) {
            response.sendRedirect(request.getContextPath() + "/index.jsp?param=error");
        }

        //Login successful
        AccessController.setLoggedUser(request, Objects.requireNonNull(loggedUser));    // add logged user info to session var
        ClientRedirector.redirectToMainPage(request, response);     // redirect to main page
    }

}
