package it.unipi.dsmt.jakartaee.app.interfaces;

import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import jakarta.ejb.Remote;
import jakarta.validation.constraints.NotNull;
import java.util.List;


@Remote
public interface WhiteboardEJB {
    List<MinimalWhiteboardDTO> searchWhiteboard(@NotNull String name, @NotNull String userID);
    List<MinimalWhiteboardDTO> getAllWhiteboards(@NotNull String userID);
    List<MinimalWhiteboardDTO> getSharedWhiteboards(@NotNull String ID);
    MinimalWhiteboardDTO getWhiteboardByID(@NotNull int whiteboardID);
    int addWhiteboard(@NotNull String ownerID, WhiteboardCreationDTO newWhiteboard);
    boolean deleteWhiteboard(String whiteboardID);
    List<String> getParticipantUsernames(@NotNull int whiteboardID);
    boolean isWhiteboardOwner(@NotNull String userId, @NotNull String whiteboardId);
    ParticipantOperationStatus removeParticipant(@NotNull String userId, @NotNull String whiteboardId);
    ParticipantOperationStatus isParticipant(String username, String whiteboardID);
    ParticipantOperationStatus addParticipant(String username, String whiteboardID);
    boolean updateWhiteboardSnapshot(@NotNull byte[] snapshotDataBytes, @NotNull String whiteboardID);
    byte[] getSnapshotByWhiteboardID(@NotNull String whiteboardID);
}
