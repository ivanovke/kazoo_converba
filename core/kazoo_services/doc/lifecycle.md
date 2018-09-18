# Service Lifecycle

To show lifecyle of service here are the anticipated scenarios of service changes being made and the resulting charges.

Let's assume an account tree of:

```
    Master
    |- Reseller1
       |- Reseller2
          |- Client3
       |- Client2
    |- Client1
```

Here we have a master account **Master** with two children, **Reseller1** (a reseller) and **Client1** (a direct client). **Reseller1**, in turn, has two children **Reseller2** (another reseller) and **Client2** (a direct client). Finally, **Reseller2** has one child, **Client3** (a direct client).

It is important to keep in mind not only which account is changing a service quantity but also __who__ is making the change (an account user, reseller user, or master user).

## Master Account

Since **Master** is the master account, it doesn't really get a service plan applied to it and can do as it pleases. Phew, that was easy.

## Create Reseller1

When **Master** creates **Reseller1**, it applies a service plan (or several) to the account. Let's use a super simple service plan (SSSP) that only charges $1 per device. After promoting the account to reseller through [API](https://github.com/2600hz/kazoo/blob/KAZOO-5970/applications/crossbar/doc/accounts.md#promote-a-reseller) or using SUP command `sup kazoo_services_maintenance make_reseller {R1_ACCOUNT_ID}`, **Reseller1** now makes sub-accounts for itself.

### Resller1 Makes Changes

Now that **Reseller1** exists and is a reseller, let's look at when the **Reseller1** admin user creates a device and see what should occur.

When attempting to [create a device](https://github.com/2600hz/kazoo/blob/KAZOO-5970/applications/crossbar/doc/devices.md#create-a-new-device), Crossbar returns a 402 a payload similar to (basically since the account has a service plan applied into it means any billable action through API should confirm the charges being made):

```json
{
    "data": [
        {
            "items": [
                {
                    "category": "devices",
                    "item": "sip_device",
                    "quantity": 1,
                    "billable": 1,
                    "rate": 1,
                    "total": 1,
                    "changes": {
                        "type": "modified",
                        "difference": {
                            "quantity": 1
                        }
                    }
                }
            ],
            "activation_charges": [],
            "taxes": [],
            "summary": {
                "today": 0,
                "recurring": 1
            },
            "plan": {
                "devices": {
                    "sip_device": {
                        "rate": 1
                    }
                }
            }
        }
    ],
    "error": "402",
    "message": "accept charges",
    "status": "error",
    "node": "zXvZ1MrlEbxaCB926LGDtg",
    "request_id":"{REQUEST_ID}",
    "auth_token":"{AUTH_TOKEN}"
}
```

The payload shows the details of what the change would entail (in absolute terms). If the request is resubmitted with the `accept_charges` flag set to `true`, and the account is in good standing, the device is created and the services for **Reseller1** are updated.

If another device creation would occur, the 402 response would be:

```json
{
    "data": [
        {
            "items": [
                {
                    "category": "devices",
                    "item": "sip_device",
                    "quantity": 2,
                    "billable": 2,
                    "rate": 1,
                    "total": 2,
                    "changes": {
                        "type": "modified",
                        "difference": {
                            "quantity": 1
                        }
                    }
                }
            ],
            "activation_charges": [],
            "taxes": [],
            "summary": {
                "today": 0,
                "recurring": 2
            },
            "plan": {
                "devices": {
                    "sip_device": {
                        "rate": 1
                    }
                }
            }
        }
    ],
    "error": "402",
    "message": "accept charges",
    "status": "error",
    "node": "zXvZ1MrlEbxaCB926LGDtg",
    "request_id":"{REQUEST_ID}",
    "auth_token":"{AUTH_TOKEN}"
}
```

You can see that the quantity is now 2 for SIP devices. To perform the creation the request should be repeated with resubmitted with the `accept_charges` flag set to `true`.

### Master Makes Changes to Reseller1

Suppose **Master** account wants to make a change while masquerading as **Reseller1**. What happens?

Creating device #3 we see that the request is not responded to with a 402 as before. Why? Since the authenticated account of the request (in this case an admin user from **Master** account) is a reseller, the service doc of **Master** is used to process the request, and since **Master** is the boss, the request is processed and the device was saved.

If we get a services summary of **Reseller1**, we see `"sip_device"` is equal to 3 as expected. **Reseller1**, when synced with the bookkeeper, will be billed for that third device, so admins of **Master** need to be aware when making this service changes.

## Client1 is Born

So what happens to direct accounts of the master account? Again, **Master** creates **Client1** and assigns the $1 device service plan. Unlike **Reseller1**, **Client1** won't be flagged as a reseller.

### Client1 Makes Changes

Just as when **Reseller1** created its first device, **Client1** tries to create the device and gets a 402 response with the summary of what the result would be in terms of service changes.

### Master Makes Changes to **Client1**

Again, as with **Reseller1**, what **Master** wants, **Master** gets.

## Client2

**Reseller1** has signed up their first account, **Client2**. This is a normal, direct-client account. No service plan has been assigned to **Client2**. Let's see how service changes work!

### Client2 Makes Changes

When a user from **Client2** creates a SIP device, the request is processed right away, since there's no service plan to process for **Client2**.

### Reseller1 Makes Changes to Client2

**Reseller1** will be prompted to accept charges. The quantity presented will vary depending on if `"cascade": true` is on the service item.  If present the quantity will reflect the sum of all instances of that item on the **Reseller1** account and all its sub-accounts.

## Reseller2

**Reseller1** has been given reseller permissions to create their own sub accounts. Through some deal with **Master**, **Reseller1** creates a sub account **Reseller2** and instructs **Master** to mark **Reseller2** as a reseller. Do note that we haven't assigned a service plan to this account.

### Reseller2 Makes Changes

Let's see what happens when **Reseller2** creates its first device!

Interestingly, **Reseller2** is not prompted to accept charges and the device is created. This seems to be a result of not having a service plan associated with **Reseller2** and **Reseller2** being a reseller. Since the change would not result in a charge for **Reseller2** (according to Kazoo), the operation is successful.

### Reseller1 Makes Changes to Reseller2

When **Reseller1** tries to create a device in **Reseller2**, **Resller1**'s service plans are used, a 402 "accept charges" is kicked back with 4 SIP devices as the quantity because **Reseller1** has 3 devices at this point. This doesn't account for **Reseller2**'s existing device because the service plan item for `sip_device` does not include the `"cascade": true` flag. Setting the flag would result in **Resller2** seeing 5 devices in their dry run synopsis.

