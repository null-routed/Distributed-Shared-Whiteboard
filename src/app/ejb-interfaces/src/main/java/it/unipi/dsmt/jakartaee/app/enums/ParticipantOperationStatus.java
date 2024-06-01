package it.unipi.dsmt.jakartaee.app.enums;


/**
 * Enumerates the possible operation statuses when performing participant-related operations. <br>
 * - {@code SQL_SUCCESS}: The SQL operation was successful. <br>
 * - {@code UNREGISTERED_USER}: The user is not registered. <br>
 * - {@code ALREADY_PARTICIPATING}: The user is already participating in the operation. <br>
 * - {@code NOT_PARTICIPATING}: The user is not participating in the operation. <br>
 * - {@code OTHER_ERROR}: Other errors occurred during the operation. <br>
 */
public enum ParticipantOperationStatus {
    SQL_SUCCESS,
    UNREGISTERED_USER,
    ALREADY_PARTICIPATING,
    NOT_PARTICIPATING,
    OTHER_ERROR
}
