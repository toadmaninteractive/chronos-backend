%% Constants
-define(null, null).

%% Actors
-define(actor_root, root).
-define(actor_personnel, personnel).
-define(actor_client, client).
-define(actor_robot, robot).
-define(actor_anonymous, anonymous).

%% Entities
-define(ent_settings, settings).
-define(ent_personnel, personnel).
-define(ent_client, client).
-define(ent_game, game).
-define(ent_game_build, game_build).
-define(ent_game_manifest, game_manifest).
-define(ent_game_branch, game_branch).
-define(ent_game_branch_unlock, game_branch_unlock).
-define(ent_game_ownership, game_ownership).

%% Operations
-define(op_create, create).
-define(op_read, read).
-define(op_update, update).
-define(op_delete, delete).
-define(op_undelete, undelete).
-define(op_block, block).
-define(op_unblock, unblock).
-define(op_register, register).
-define(op_register_confirm, register_confirm).
-define(op_phone_confirm, phone_confirm).
-define(op_password_reset, password_reset).
-define(op_password_reset_confirm, password_reset_confirm).
-define(op_login, login).
-define(op_logout, logout).
-define(op_expire, expire).
-define(op_purchase, purchase).
-define(op_grant, grant).
-define(op_revoke, revoke).

% Client requests
-define(cr_register_confirm, register_confirm).
-define(cr_phone_confirm, phone_confirm).
-define(cr_password_reset_confirm, password_reset_confirm).

%% User state
-define(state_not_authenticated, not_authenticated).
-define(state_not_authorized, not_authorized).
-define(state_authorized, authorized).

%% Request errors
-define(err_invalid_fields, invalid_fields).
-define(err_not_authenticated, not_authenticated).
-define(err_not_authorized, not_authorized).
-define(err_not_blocked, blocked).
-define(err_deleted, deleted).
-define(err_not_exists, not_exists).
-define(err_rev_mismatch, rev_mismatch).
-define(err_invalid_credentials, invalid_credentials).
-define(err_invalid_recaptcha, invalid_recaptcha).
-define(err_account_deleted, account_deleted).
-define(err_account_blocked, account_blocked).
-define(err_already_logged_in, already_logged_in).
-define(err_already_exists, already_exists).
-define(err_nothing_to_update, nothing_to_update).
-define(err_not_updated, not_updated).

%% Field errors
-define(err_field_invalid_type, invalid_type).
-define(err_field_invalid_enum, invalid_enum).
-define(err_field_is_null, is_null).
-define(err_field_ref_not_exists, ref_not_exists).
-define(err_field_ref_already_exists, ref_already_exists).
-define(err_field_is_empty, is_empty).
-define(err_field_too_short, too_short).
-define(err_field_too_long, too_long).
-define(err_field_regex_mismatch, regex_mismatch).
-define(err_field_too_low, too_low).
-define(err_field_too_high, too_high).
-define(err_field_invalid_date, invalid_date).

%% Modifiers
-define(mod_inc, inc).
-define(mod_set_now, set_now).
-define(mod_trim, trim).
-define(mod_lower, lower).
-define(mod_lower_trim, lower_trim).
-define(mod_upper, upper).
-define(mod_upper_trim, upper_trim).
-define(mod_coalesce, coalesce).

%% Modifier construction
-define(mk_mod_inc(Field), {Field, ?mod_inc}).
-define(mk_mod_set_now(Field), {Field, ?mod_set_now}).
-define(mk_mod_trim(Field), {Field, ?mod_trim}).
-define(mk_mod_lower(Field), {Field, ?mod_lower}).
-define(mk_mod_lower_trim(Field), {Field, ?mod_lower_trim}).
-define(mk_mod_upper(Field), {Field, ?mod_upper}).
-define(mk_mod_upper_trim(Field), {Field, ?mod_upper_trim}).
-define(mk_mod_coalesce(Field), {Field, ?mod_coalesce}).
