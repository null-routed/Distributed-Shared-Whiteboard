package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.SignupDTO;
import it.unipi.dsmt.jakartaee.app.enums.SignupStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.Optional;


/**
 * Servlet handling POST requests for login.
 */
@WebServlet(name = "SignupServlet", value = "/signup")
public class SignupServlet extends HttpServlet {

    private String messageToJSPPage;

    @EJB
    private UserEJB userEJB;

    private boolean checkSignupParameters(String name, String surname, String username, String password, String repeatPassword) {
        final int USERNAME_NAME_MAX_LENGTH = 50;
        final int SURNAME_MAX_LENGTH = 100;
        final int PASSWORD_MIN_LENGTH = 8;

        if (name.length() > USERNAME_NAME_MAX_LENGTH) {
            messageToJSPPage = "Name can be at most " + USERNAME_NAME_MAX_LENGTH + " characters long.";
            return false;
        }

        if (surname.length() > SURNAME_MAX_LENGTH) {
            messageToJSPPage = "Surname can be at most " + SURNAME_MAX_LENGTH + " characters long.";
            return false;
        }
        if (username.length() > USERNAME_NAME_MAX_LENGTH) {
            messageToJSPPage = "Username can be at most " + USERNAME_NAME_MAX_LENGTH + " characters long.";
            return false;
        }

        if (password.length() < PASSWORD_MIN_LENGTH) {
            messageToJSPPage = "Password must be at least " + PASSWORD_MIN_LENGTH + " characters long.";
            return false;
        }

        if (!password.equals(repeatPassword)) {
            messageToJSPPage = "Passwords do not match.";
            return false;
        }

        return true;
    }

    /**
     * Handle a POST request by executing the signup procedure.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("SignupServlet: called doPost() method...");

        // Extract parameters for signup
        String name = Optional.ofNullable(request.getParameter("name")).orElse("");
        String surname = Optional.ofNullable(request.getParameter("surname")).orElse("");
        String email = Optional.ofNullable(request.getParameter("email")).orElse("");
        String username = Optional.ofNullable(request.getParameter("username")).orElse("");
        String password = Optional.ofNullable(request.getParameter("password")).orElse("");
        String repeatPassword = Optional.ofNullable(request.getParameter("password-repeat")).orElse("");

        boolean checkOutcome = checkSignupParameters(name, surname, username, password, repeatPassword);
        if (!checkOutcome) {
            request.setAttribute("signupError", true);      // letting signup.jsp know there has been an error with parameters
            request.setAttribute("errorMessage", messageToJSPPage);     // setting error message that will be retrieved by signup page
            response.sendRedirect(request.getContextPath() + "/signup.jsp");
        }

        // Execute signup through EJB
        SignupDTO signupDTO = new SignupDTO(name, surname, email, username, password);
        SignupStatus signupOperationOutcome = userEJB.signup(signupDTO);

        // choosing what to do based on the signup outcome
        switch (signupOperationOutcome) {
            case DUPLICATE_USERNAME:
                messageToJSPPage = "The username is already taken. Choose another one.";
                request.setAttribute("signupError", true);
                request.setAttribute("errorMessage", messageToJSPPage);
                break;
            case DUPLICATE_EMAIL:
                messageToJSPPage = "An account is already registered with this Email address.";
                request.setAttribute("signupError", true);
                request.setAttribute("errorMessage", messageToJSPPage);
                break;
            case OTHER_ERROR:
                messageToJSPPage = "A problem occurred during the signup procedure. Try again or try in a few minutes.";
                request.setAttribute("signupError", true);
                request.setAttribute("errorMessage", messageToJSPPage);
                break;
            case SUCCESS:
                messageToJSPPage = "Your account was successfully created. Login to use the application!";
                request.setAttribute("signupError", false);
                request.setAttribute("successMessage", messageToJSPPage);
                break;
        }

        response.sendRedirect(request.getContextPath() + "/signup.jsp");
    }

    /**
     * Handles a GET request by redirecting to the signup page.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    @Override
    protected void doGet (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        System.out.println("SIGNUPSERVLET: called doGet() method");
        String targetPage = request.getContextPath() + "/WEB-INF/jsp/signup.jsp";
        System.out.println("SIGNUPSERVLET: redirecting to " + targetPage);
        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
