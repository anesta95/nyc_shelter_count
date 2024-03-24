import os
import datetime
import sys

def get_formatted_date_two_months_ago():
    today = datetime.date.today()
    # Calculate two months ago
    two_months_ago = today - datetime.timedelta(days=60)
    # Format the date as MM_YYYY
    formatted_date = two_months_ago.strftime("%m_%Y")
    return formatted_date

formatted_date = get_formatted_date_two_months_ago()
print("Date two months ago (MM_YYYY):", formatted_date)

def rename_file(formatted_date):
    # Define the original file name
    original_file = "temporary_housing_report_pdfs/temp_temporary_housing_report.pdf"

    # Define the new file name with the formatted date
    new_file = f"temporary_housing_report_pdfs/temporary_housing_report_{formatted_date}.pdf"

    try:
        # Rename the file
        os.rename(original_file, new_file)
        print(f"File successfully renamed to: {new_file}")
    except FileNotFoundError:
        print("Error: File not found.")
    except FileExistsError:
        print("Error: A file with the new name already exists.")


if os.path.exists(f"temporary_housing_report_pdfs/temporary_housing_report_{formatted_date}.pdf"):
    print(f"Error: File for '{formatted_date}' already exists.")
else:
    rename_file(formatted_date)