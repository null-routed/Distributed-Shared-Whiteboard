package it.unipi.dsmt.jakartaee.app.interfaces;

import it.unipi.dsmt.jakartaee.app.dto.*;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

/**
 * Interface for EJBs responsible for handling all the business logic related to the user management.
 */
@Remote
public interface UserEJB {
    LoggedUserDTO login (@NotNull LoginInformationsDTO loginInformationDTO);
    boolean signup(SignupDTO signupDTO);
}
