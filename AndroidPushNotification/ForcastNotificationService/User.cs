using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ForcastNotificationService
{
    public class User
    {
        public long UserId { get; set; }

        public string UserName { get; set; }

        public string RegistrationId { get; set; }

        public bool isActive { get; set; }

        public string EmailId { get; set; }

        public string MacId { get; set; }

        public long MobileNo { get; set; }

        public string DeviceId { get; set; }
    }
}
