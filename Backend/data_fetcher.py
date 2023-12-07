import mysql.connector
from datetime import datetime, timedelta
import yfinance as yf

def get_dollar_prices():
    db_connection = mysql.connector.connect(
        host='localhost',
        user='root',
        password='19175',
        database='anfin',
        port=3306,
        charset='utf8'
        )
    
    cursor = db_connection.cursor()
    # Use yfinance to get historical exchange rate data for USD to MXN
    currency_pair = 'USDMXN=X'
    end_date = datetime.today().strftime('%Y-%m-%d')
    start_date = (datetime.today() - timedelta(days=3)).strftime('%Y-%m-%d')
    historical_data = yf.download(currency_pair, start=start_date, end=end_date)

    # Extract closing prices for each day within the date range
    dollar_prices = historical_data['Close']

    db_connection.commit()
    cursor.close()
    db_connection.close()

    return dollar_prices

def get_dollar_price():
    db_connection = mysql.connector.connect(
        host='localhost',
        user='root',
        password='19175',
        database='anfin',
        port=3306,
        charset='utf8'
        ) 
    cursor = db_connection.cursor()
    # Use yfinance to get historical exchange rate data for USD to MXN
    currency_pair = 'USDMXN=X'
    end_date = datetime.today().strftime('%Y-%m-%d')
    start_date = (datetime.today() - timedelta(days=3)).strftime('%Y-%m-%d')
    historical_data = yf.download(currency_pair, start=start_date, end=end_date)

    # Extract closing prices for each day within the date range
    dollar_prices = historical_data['Close']

    db_connection.commit()
    cursor.close()
    db_connection.close()
    return dollar_prices

def store_dollar_prices(cursor, dollar_prices):

    try:
        db_connection = mysql.connector.connect(
        host='localhost',
        user='root',
        password='19175',
        database='anfin',
        port=3306,
        charset='utf8'
        ) 
        cursor = db_connection.cursor()
        for date, price in dollar_prices.items():
            # Check for duplicate entries for the current date
            sql_check_duplicate = f"SELECT * FROM dollar_prices WHERE date = '{date.date()}'"
            cursor.execute(sql_check_duplicate)
            duplicate_data = cursor.fetchall()

            if not duplicate_data:
                # If no duplicate, insert the new data into the database
                sql_insert = f"INSERT INTO dollar_prices (date, price) VALUES ('{date.date()}', {price})"
                cursor.execute(sql_insert)
                print(f'Dollar price for {date.date()} stored successfully.')

            else:
                print(f'Dollar price for {date.date()} already exists in the database.')
        db_connection.commit()
        cursor.close()
        db_connection.close()

        return True
        
    except Exception as ex:
        print(f'Error storing dollar prices in the database: {ex}')
        return False

if __name__ == '__main__':
    # Set up MySQL connection
    db_connection = mysql.connector.connect(
        host='localhost',
        user='root',
        password='19175',
        database='anfin',
        port=3306,
        charset='utf8'
    )

    cursor = db_connection.cursor()

    # Get dollar prices and store them in the database
    dollar_prices = get_dollar_price()
    if not dollar_prices.empty:
        store_dollar_prices(cursor, dollar_prices)
    print(dollar_prices)
    # Commit changes and close the cursor and database connection
    db_connection.commit()
    cursor.close()
    db_connection.close()
