#!/usr/bin/env python3

import os

def get_hackmode_operation_env():
    return os.getenv("HACKMODE_OP")

def get_hackmode_operation_path_env():
    return os.getenv("HACKMODE_PATH")


def get_hackmode_files_path(global_setting=False):
    if global_setting:
        return os.path.expanduser("~/.local/share/hackmode/")
    else:
        return os.path.join(os.getenv("HACKMODE_PATH"), ".config/")


def get_current_operation_file_path():
    return os.path.join(get_hackmode_files_path(), "current-op")


def get_current_path_to_operation_file_path():
    return os.path.join(get_hackmode_files_path(), "op-path")


def get_user_ids_file_path():
    return os.path.join(get_hackmode_files_path(), ".config/user-ids")


def read_user_ids():
    user_ids_file_path = get_user_ids_file_path()
    if os.path.exists(user_ids_file_path):
        with open(user_ids_file_path, "r") as file:
            return file.read().splitlines()
    else:
        return []


# Example usage:
operation_env = get_hackmode_operation_env()
operation_path_env = get_hackmode_operation_path_env()
files_path = get_hackmode_files_path()
current_operation_file_path = get_current_operation_file_path()
current_path_to_operation_file_path = get_current_path_to_operation_file_path()
user_ids = read_user_ids()

# Print the values for demonstration
print(f"Hackmode Operation: {operation_env}")
print(f"Hackmode Operation Path: {operation_path_env}")
print(f"Hackmode Files Path: {files_path}")
print(f"Current Operation File Path: {current_operation_file_path}")
print(f"Current Path to Operation File Path: {current_path_to_operation_file_path}")
print(f"User IDs: {user_ids}")
