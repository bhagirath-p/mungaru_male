using PushNotification;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Runtime.Serialization;
using System.ServiceModel;
using System.Text;

namespace ForcastNotificationService
{
    public class Service1 : IService1
    {
        public void ReceiveDayStatus(string value)
        {
            if (value.ToLower() == "d")
                SendMessageToAll();
        }

        private void SendMessageToAll()
        {
            List<User> users = new List<User>();
            users = GetAllUsers();
            if(users != null)
            {
                Android android = new Android();
                foreach (var usr in users)
	            {
                    android.SendMessage(usr.RegistrationId,"Something is wrong today", "");
	            }
            }
        }

        private List<User> GetAllUsers()
        {
            SqlCommand cmd = new SqlCommand();
            User user;
            List<User> users;
            cmd.CommandText = "UspGetAllUsersInfo";
            cmd.CommandType = System.Data.CommandType.StoredProcedure;
            try
            {
                users = new List<User>();
                using (SqlConnection con = new SqlConnection())
                {
                    con.ConnectionString = "";
                    con.Open();
                    var reader = cmd.ExecuteReader();
                    if (reader.HasRows)
                    {
                        while (reader.Read())
                        {
                            user = new User();
                            Int32 ordinal;

                            ordinal = reader.GetOrdinal("RegistrationID");
                            if (!reader.IsDBNull(ordinal))
                                user.RegistrationId = Convert.ToString(reader[ordinal]);

                            ordinal = reader.GetOrdinal("MacID");
                            if (!reader.IsDBNull(ordinal))
                                user.MacId = Convert.ToString(reader[ordinal]);

                            ordinal = reader.GetOrdinal("PhoneNo");
                            if (!reader.IsDBNull(ordinal))
                                user.MobileNo = Convert.ToInt32(reader[ordinal]);

                            ordinal = reader.GetOrdinal("isActive");
                            if (!reader.IsDBNull(ordinal))
                                user.isActive = Convert.ToBoolean(reader[ordinal]);

                            ordinal = reader.GetOrdinal("EmailID");
                            if (!reader.IsDBNull(ordinal))
                                user.EmailId = Convert.ToString(reader[ordinal]);

                            ordinal = reader.GetOrdinal("DeviceID");
                            if (!reader.IsDBNull(ordinal))
                                user.DeviceId = Convert.ToString(reader[ordinal]);

                            users.Add(user);
                            user = null;
                        }
                    }
                    con.Close();
                }
                return users;
            }
            catch (Exception ex)
            {
                throw;
                return null;
            }
            finally
            {
                cmd = null;
            }
        }
    }
}
