package it.unipi.dsmt.jakartaee.app.enums;


/**
 * Enumerates the possible signup statuses when registering a new user. <br>
 * - {@code SUCCESS}: The signup operation was successful. <br>
 * - {@code DUPLICATE_USERNAME}: The provided username already exists. <br>
 * - {@code DUPLICATE_EMAIL}: The provided email address is already associated  with an existing account. <br>
 * - {@code OTHER_ERROR}: Other errors occurred during the signup operation. <br>
 */
public enum SignupStatus {
    SUCCESS,
    DUPLICATE_USERNAME,
    DUPLICATE_EMAIL,
    OTHER_ERROR
}
